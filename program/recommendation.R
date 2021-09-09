require(shiny)
# customSentence_share <- function(numItems, type) {
#   paste("Love it? Share it!")
# }
# dropdownMenuCustom <-function (..., type = c("messages", "notifications", "tasks"),
#                                badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence)
# {
#   type <- match.arg(type)
#   if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
#   items <- c(list(...), .list)
#   lapply(items, shinydashboard:::tagAssert, type = "li")
#   dropdownClass <- paste0("dropdown ", type, "-menu")
#   if (is.null(icon)) {
#     icon <- switch(type, messages = shiny::icon("envelope"),
#                    notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
#   }
#   numItems <- length(items)
#   if (is.null(badgeStatus)) {
#     badge <- NULL
#   }
#   else {
#     badge <- tags$span(class = paste0("label label-", badgeStatus),
#                        numItems)
#   }
#   tags$li(
#     class = dropdownClass,
#     a(
#       href = "#",
#       class = "dropdown-toggle",
#       `data-toggle` = "dropdown",
#       icon,
#       badge
#     ),
#     tags$ul(
#       class = "dropdown-menu",
#       tags$li(
#         class = "header",
#         customSentence(numItems, type)
#       ),
#       tags$li(
#         tags$ul(class = "menu", items)
#       )
#     )
#   )
# }
display_card <- function(card, height_ = 130){
  filename <- normalizePath(file.path(paste0('www/card/', card, '.png')))
  return(list(src = filename, height = height_))
}

rule_model <- function(user_info = input_vec){
  print(user_info)
  ubear = c(0.9, -1, 0, -0.2, -0.2)
  only = c(0.6, -0.89, 0, -0.3, -0.44)
  pi = c(0.87, -0.6, 0, -0.34, -0.53)
  nanshan = c(0, 0.7, 0.9, 0,0)
  careforr = c(0,0,0.95, 0, 0.86)
  jcb = c(0, 0.43, 0, 0.45, 0.3)
  dual = c(0, 0.2, 0, 0.3, 0.62)
  world = c(-0.2, 0.3, 0.9, 0.3, 0.5)
  # user_info = c(2, 0.2, 0, 0.1, 0.1)

  score.ubear = ubear %*% user_info
  score.only = only %*% user_info
  score.pi = pi %*% user_info
  score.nanshan = nanshan %*% user_info
  score.careforr = careforr %*% user_info
  score.jcb = jcb %*% user_info
  score.dual = dual %*% user_info
  score.world = world %*% user_info

  scores = c('ubear' = score.ubear,  'only' = score.only, 'pi' = score.pi,
             'nanshan' = score.nanshan, 'careforr' = score.careforr,
             'jcb' = score.jcb, 'dual' = score.dual, 'world' = score.world)
  ## score phase2
  ubear = c(0.9, -1, 0, -0.2, -0.2)
  only = c(0.6, -0.89, 0, -0.3, -0.44)
  pi = c(0.87, -0.6, 0, -0.34, -0.53)
  nanshan = c(0, 0.7, 0.9, 0,0)
  careforr = c(0,0,0.95, 0, 0.86)
  jcb = c(0, 0.43, 0, 0.45, 0.3)
  dual = c(0, 0.2, 0, 0.3, 0.62)
  world = c(-0.2, 0.3, 0.9, 0.3, 0.5)

  top3 = scores %>%sort(decreasing  = T) %>% head(3) %>%names()
  return(top3)

}

recommend <- function(card){
  switch (card,
    'ubear' = HTML('<h3><span style="background-color: #ffffff; color: #800000;"><strong>玉山Ubear信用卡</strong></span></h3>
<p><span style="color: #800000;">◎一般消費1%<br /></span><span style="color: #800000;">◎網購3.8%</span><br /><span style="color: #800000;">◎指定娛樂/影音平台12%現金回饋</span></p>'),
    'careforr' = HTML('<h3><span style="background-color: #ffffff; color: #009e8e;"><strong>家樂福悠遊聯名卡</strong></span></h3>
<p><span style="color: #000000;">◎悠遊 ETC超方便<br /></span><span style="color: #000000;">◎感應刷卡免簽名<br /></span><span style="color: #000000;">◎點數變現金 購物省最多</span></p>'),
    'pi' = HTML('<h3><span style="color: #009e8e;">玉山Pi拍錢包信用卡</span></h3>
<p><span style="color: #000000;">◎新戶享首刷禮500 P幣<br />◎保費享 1.5% P幣回饋無上限<br />◎指定通路最高享5% P幣回饋</span></p>'),
    'nanshan' = HTML('<h3><span style="background-color: #ffffff; color: #009e8e;"><strong>南山人壽聯名卡</strong></span></h3>
<p><span style="color: #000000;">◎南山保費享1%折扣或現金回饋<br />◎南山保費享0.2%現金回饋<br />◎提撥消費金額0.1%，支持南山慈善基金醫療關懷計畫</span></p>'),
    'dual' = HTML('<h3><span style="background-color: #ffffff; color: #800000;"><strong>玉山雙幣信用卡</strong></span></h3>
<p><span style="color: #800000;">◎免收1.5%國外交易服務費<br /></span><span style="color: #800000;">◎國外消費以外幣繳款<br /></span><span style="color: #800000;">◎國內消費享最高0.6%現金回饋</span></p>')
,
    'only' = HTML('<h3><span style="background-color: #ffffff; color: #800000;"><strong>玉山Only卡</strong></span></h3>
<p><span style="color: #800000;">◎指定通路加贈5倍紅利<br /></span><span style="color: #800000;">◎紅利倍多芬，最高享有13倍紅利<br /></span><span style="color: #800000;">◎淘寶99划算節 最高享20%回饋</span></p>')
,
    'world' = HTML('<h3><span style="background-color: #ffffff; color: #009e8e;"><strong>玉山世界卡</strong></span></h3>
<p><strong><span style="background-color: #ffffff; color: #000000;">玉山VIP的身分證</span></strong></p>
<p><span style="color: #000000;">◎新戶享國內一般消費5%現金回饋<br />◎玉山世界卡專屬換匯優惠<br />◎國外消費最高享3%現金回饋</span></p>')
)
}
