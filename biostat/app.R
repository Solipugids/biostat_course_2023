library(shiny)
library(aws.s3)
library(shinythemes)

s3BucketName <- "bi0stat2023"
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIASILQ4NURHNE2OBU7",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "ap-southeast-1")

# Define the fields we want to save from the form
fields <- c("name", "student_id", "class", "assignment")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    theme = shinytheme("spacelab"),
    titlePanel("生物试验设计课程作业提交", windowTitle = "作业提交"),
    HTML("<p><a href='https://gitee.com/cha0w/biostat_course_2023'> &rarr;	课程教学资源地址：gitee.com/cha0w/biostat_course_2023 &larr;	</a></p>"),
    HTML("<p><b>QQ群：</b></p>"),
    HTML("<p>海洋药学22：862588942</p>"),
    HTML("<p>生物工程类 22(1)，生物工程类 22(2)：741120411</p><br>"),
    textInput("name", "姓名", "", width="70%", placeholder = "请输入本人姓名"),
    textInput("student_id", "学号", "", width="70%", placeholder = "请输入本人学号"),
    selectInput("class", "选择你的班级", choices = c("请选择你的班级"="", "海药22", "生工22（1）", "生工22（2）"), multiple = FALSE, width = "70%"),
    HTML(paste("<b>", "提交日期：", as.character(Sys.Date()), "</b><br>")),
    HTML(paste("<b>", "作业题目：", "</b><br>")),
    HTML("<p style='color:#FF0000';>请论述在非线性回归中，如何量化及评估曲线函数拟合的质量？</p>"),
    textAreaInput("assignment", "提交作业", value = "", width = "70%", height = "400px", placeholder = "此处输入作业答案"),
    HTML("<p style='color:#FF0000';>以上内容均为必填项，请检查是否填写完整并点击“上传提交”</p>"),
    actionButton("submit", "上传"),

    tags$hr(),
    HTML("<mark><b>", "已完成提交实时更新列表", "</b></mark>"),
    DT::dataTableOutput("responses", width = "70%"),
    tags$hr(),
    tags$footer("提交过程中遇到问题请邮件联系：wangchao@gdpu.edu.cn。",
    align = "left",
    style = "
    position:relative;
    bottom:0;
    width:100%;
    height:50px;   /* Height of the footer */
    color: white;
    padding: 10px;
    background-color: #071274;
    z-index: 1000;"
    )
  ),
  
  
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDT({
      input$submit
      loadData()
    }
    )
    
    outputDir <- "assignments"
    
    saveData <- function(data) {
  # Create a plain-text representation of the data
  data <- paste0(
    paste(names(data), collapse = ","), "\n",
    paste(unname(data), collapse = ",")
  )

  file_name <- paste0(
    paste(
      get_time_human(),
      digest(data, algo = "md5"),
      sep = "_"
    ),
    ".csv"
  )
 # Upload the file to S3
  put_object(file = charToRaw(data), object = file_name, bucket = s3_bucket_name)
}

    
    loadData <- function() {
  # Get a list of all files
  file_names <- get_bucket_df(s3BucketName)[["Key"]]
  # Read all files into a list
  data <- lapply(file_names, function(x) {
    object <- get_object(x, s3BucketName)
    object_data <- readBin(object, "character")
    read.csv(text = object_data, stringsAsFactors = FALSE)
  })
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  if (is.data.frame(data)){
        data <- data[, 1:3]
        colnames(data) <- c("姓名", "学号", "班级")
      }
  data  
    }

  }
)