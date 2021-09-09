source('program//recommendation.R',encoding = "utf-8")

server <- function(input, output, session) {

  output$questionare1 <- renderUI({
    shinydashboardPlus::box(width = NULL, h4('您的資料'),
                            radioButtons(inputId = 'sex', label = '性別',
                                         choices = c('男' = 1,'女' = 2,'其他' = 0), inline = T),
                            numericInput(inputId = 'age', label = '年齡', value = 25, min = 18, max = 99),
                            radioButtons(inputId = 'marriage', label = '婚姻狀況', choices = c('已婚' = 1,'未婚' = 0,'其他' = 0),inline =T),
                            selectInput(inputId = 'industry', label = '工作產業',
                                        choices = c('農林漁牧業', '製造業','營建工程業','批發及零售業','運輸及倉儲業',
                                                    '住宿及餐飲業', '金融及保險業', '教育業',
                                                    '醫療保健業', '藝術、娛樂及休閒服務業', '其他服務業'),
                                        selected = '金融及保險業'),
                            selectInput(inputId = 'title', label = '職稱',
                                        choices = c('部(門)處長','經理','襄理','工程師','職員',
                                                    '主任/資深職員','專案經理','小組長', '資深工程師'),
                                        selected = '資深工程師'),
                            selectInput(inputId = 'income', label = '年收入範圍',
                                        choices = c('30萬以下' = 0, '30萬-70萬' = 1/3, '70萬-100萬' = 2/3,'100萬以上'= 3/3),
                                        selected = '100萬以上')
                            )
  })
  output$questionare2 <- renderUI({
    shinydashboardPlus::box(width = NULL, h4('30秒快速問答'),
                            radioButtons(inputId = 'Q1', label = 'Q1. 你比較喜歡...',
                                         choices = c('出去逛逛' = -1, '中立意見' = 0,'在家滑網拍' = 1), inline = T),
                            radioButtons(inputId = 'Q2', label = 'Q2. 你總是...',
                                         choices = c('我只購買我需要的東西' = -1, '中立意見' = 0,'我看到喜歡的就買了' = 1), inline = T),
                            radioButtons(inputId = 'Q3', label = 'Q3. 你比較喜歡...',
                                         choices = c('我喜歡冒險' = -1, '中立意見' = 0,'待在舒適圈讓我感到安心' = 1), inline = T),
                            radioButtons(inputId = 'Q4', label = 'Q4. 你比較喜歡...',
                                         choices = c('我喜歡研究各種回饋' = -1, '中立意見' = 0,'只要有回饋就好，懶得比較' = 1), inline = T),
                            radioButtons(inputId = 'Q5', label = 'Q5. 我喜歡海外消費',
                                         choices = c('同意' = 1, '中立意見' = 0,'不同意' = -1), inline = T),
                            radioButtons(inputId = 'Q6', label = 'Q6. 我喜歡研究就各種回饋/只要有回饋就好，懶得比較',
                                         choices = c('同意' = 1, '中立意見' = 0,'不同意' = -1), inline = T)
    )
  })

  top3  <- eventReactive(input$go, {
    input_vec = c(input$sex, input$age/100,input$marriage, 0, input$income)%>%
      as.numeric()
    # input_vec = c(input$sex, input$age / 100,input$marriage, input$title, input$income)
    res = rule_model(user_info = input_vec)
    return(res)
  })

  output$output_card1 <- renderImage({
    display_card(card = top3()[1], height_ = 130)
  }, deleteFile = F)
  output$output_card2 <- renderImage({
    display_card(card = top3()[2], height_ = 130)
  }, deleteFile = F)
  output$output_card3 <- renderImage({
    display_card(card = top3()[3], height_ = 130)
  }, deleteFile = F)
  output$result <- renderText({
    paste0(top3()[1], top3()[2], top3()[3])
  })
  output$text1 <- renderUI({
    recommend(top3()[1])
  })
  output$text2 <- renderUI({
    recommend(top3()[2])
  })
  output$text3 <- renderUI({
    recommend(top3()[3])
  })

  output$more1 <- renderUI({
    actionButton(inputId = 'understand', label = '了解更多', style="color: #009e8e; background-color: #fff; border-color: #009e8e",
                 icon = icon('credit-card'),
                 onclick ="window.open('https://www.esunbank.com.tw/bank/personal/credit-card/intro/bank-card/only-card', '_blank')")
  })
  output$more2 <- renderUI({
    actionButton(inputId = 'understand', label = '了解更多', style="color: #009e8e; background-color: #fff; border-color: #009e8e",
                 icon = icon('credit-card'),
                 onclick ="window.open('https://www.esunbank.com.tw/bank/personal/credit-card/intro/bank-card/only-card', '_blank')")
  })
  output$more3 <- renderUI({
    actionButton(inputId = 'understand', label = '了解更多', style="color: #009e8e; background-color: #fff; border-color: #009e8e",
                 icon = icon('credit-card'),
                 onclick ="window.open('https://www.esunbank.com.tw/bank/personal/credit-card/intro/bank-card/only-card', '_blank')")
  })

  output$apply1 <- renderUI({
    actionButton(inputId = 'apply1', label = '立即申請', style="color: #fff; background-color: #009e8e; border-color: #009e8e",
                 icon = icon('pencil-alt'),
                 onclick ="window.open('https://www.esunbank.com.tw/bank/personal/credit-card/intro/bank-card/only-card', '_blank')")
#     HTML('<h4><a class="btn-color brand-primary" href="https://card.esunbank.com.tw/EsunCreditweb/txnproc/preChoice?PRJCD=APYCRD0051" target="_blank">立即申辦</a></h4>
# <h4><span style="background-color: #ffffff;"><a class="btn-color" style="background-color: #ffffff;" href="https://www.esunbank.com.tw/bank/personal/credit-card/intro/bank-card/only-card" target="_blank">詳細內容</a></span></h4>')
  })
  output$apply2 <- renderUI({
    actionButton(inputId = 'apply2', label = '立即申請', style="color: #fff; background-color: #009e8e; border-color: #009e8e",
                 icon = icon('pencil-alt'),
                 onclick ="window.open('https://www.esunbank.com.tw/bank/personal/credit-card/intro/bank-card/only-card', '_blank')")
#     HTML('<h4><a class="btn-color brand-primary" href="https://card.esunbank.com.tw/EsunCreditweb/txnproc/preChoice?PRJCD=APYCRD0051" target="_blank">立即申辦</a></h4>
# <h4><span style="background-color: #ffffff;"><a class="btn-color" style="background-color: #ffffff;" href="https://www.esunbank.com.tw/bank/personal/credit-card/intro/bank-card/only-card" target="_blank">詳細內容</a></span></h4>')
  })
  output$apply3 <- renderUI({
    actionButton(inputId = 'apply3', label = '立即申請', style="color: #fff; background-color: #009e8e; border-color: #009e8e",
                 icon = icon('pencil-alt'),
                 onclick ="window.open('https://www.esunbank.com.tw/bank/personal/credit-card/intro/bank-card/only-card', '_blank')")
#     HTML('<h4><a class="btn-color brand-primary" href="https://card.esunbank.com.tw/EsunCreditweb/txnproc/preChoice?PRJCD=APYCRD0051" target="_blank">立即申辦</a></h4>
# <h4><span style="background-color: #ffffff;"><a class="btn-color" style="background-color: #ffffff;" href="https://www.esunbank.com.tw/bank/personal/credit-card/intro/bank-card/only-card" target="_blank">詳細內容</a></span></h4>')
  })

  output$default1 <- renderImage({
    filename <- normalizePath(file.path(paste0('www/default/', 'honhui', '.jpg')))
    return(list(src = filename, height = 180))
  }, deleteFile = F)
  output$default2 <- renderImage({
    filename <- normalizePath(file.path(paste0('www/default/', 'e_finger', '.jpg')))
    return(list(src = filename, height = 180))
  }, deleteFile = F)
  output$default3 <- renderImage({
    filename <- normalizePath(file.path(paste0('www/default/', 'service', '.jpg')))
    return(list(src = filename, height = 180))
  }, deleteFile = F)
  output$recPanel <- renderUI({
    column(width = 7,
           fluidRow(
             shinydashboard::box(width = NULL,height = '210px',
                                 imageOutput(outputId = 'default1', width = "60%", height = '100px')
             )
             # %>%withSpinner(type = 4,color = "#d33724",size = 0.7)
           ),
           fluidRow(
             shinydashboard::box(width = NULL, height = '210px',
                                 imageOutput(outputId = 'default2', width = "60%", height = '100px')
             )
             # %>%withSpinner(type = 4,color = "#d33724",size = 0.7)
           ),
           fluidRow(
             shinydashboard::box(width = NULL, height = '210px',
                                 imageOutput(outputId = 'default3', width = "60%", height = '100px')
             )
             # %>%withSpinner(type = 4,color = "#d33724",size = 0.7)
           )
    )
  })
  observeEvent(input$go, {
    output$recPanel <- renderUI({
      column(width = 7,
             fluidRow(
               h3('為您推薦...'),
               shinydashboard::box(width = NULL,height = '210px',
                                   column(width = 3, br(),imageOutput(outputId = 'output_card1', width = "90%", height = '100px')),
                                   column(width = 5,offset = 2,
                                          uiOutput(outputId = 'text1'),
                                          fluidRow(
                                            column(width = 5, uiOutput(outputId = 'apply1')),
                                            column(width = 5,uiOutput(outputId = 'more1'))
                                          )
                                          )
               )
             ),
             fluidRow(
               shinydashboard::box(width = NULL, height = '210px',
                                   column(width = 3, br(),imageOutput(outputId = 'output_card2', width = "90%",height = '100px')),
                                   column(width = 5, offset = 2,
                                          uiOutput(outputId = 'text2'),
                                          fluidRow(
                                            column(width = 5, uiOutput(outputId = 'apply2')),
                                            column(width = 5,uiOutput(outputId = 'more2'))
                                          )
                                          )
               )
             ),
             fluidRow(
               shinydashboard::box(width = NULL, height = '220px',
                                   column(width = 3,br(),imageOutput(outputId = 'output_card3', width = "90%",height = '100px')),
                                   column(width = 5,offset = 2,
                                          uiOutput(outputId = 'text3'),
                                          fluidRow(
                                            column(width = 5, uiOutput(outputId = 'apply3')),
                                            column(width = 5,uiOutput(outputId = 'more3'))
                                          )
                                          )
               )
             )
      )
    })
  })

}
