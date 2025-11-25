# update_knowledge.R
# 이 스크립트를 GitHub 저장소 루트에 저장해야 합니다.

library(httr)
library(rvest)
library(dplyr)
library(stringr)

# --- 설정 ---
g4k_url <- "https://www.g4k.go.kr/cipp/0200/selectCIPP0201.do?cffdnCd=CIPOK00049"
telegram_token <- Sys.getenv("TELEGRAM_BOT_TOKEN") 
telegram_chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")

# --- 1. G4K 데이터 수집 및 정제 ---
res <- httr::GET(g4k_url, httr::user_agent("GHA R Scraper"))
page <- rvest::read_html(res)

final_g4k_clean <- page %>% html_node("body") %>% html_text() %>% 
    str_remove("^.*?여권 정보 증명서 발급 발급 예외적 여권사용 허가 신청 신청 공증·아포스티유") %>%
    str_remove("개인정보 처리방침.*?이 누리집은 대한민국 공식 전자정부 누리집입니다\\.") %>%
    str_replace_all("신청 신청|발급 발급|조회 조회", " ") %>%
    str_squish()

# --- 2. 데이터프레임 구성 ---
new_data <- data.frame(
    Type = "안내문", 
    Title = "재외동포 등록 및 민원 표준 안내", 
    Content = final_g4k_clean, 
    Source = "재외동포365포털", 
    Link = g4k_url, 
    stringsAsFactors = FALSE
)

# --- 3. 텔레그램 알림 발송 ---
if (nchar(final_g4k_clean) > 6000) {
    message <- "✅ G4K 지식 베이스 갱신 성공. 최신 데이터로 업데이트되었습니다."
} else {
    message <- "🚨 G4K 지식 베이스 갱신 실패. 페이지 구조를 확인하거나 데이터가 비었습니다."
}

httr::POST(
    url = paste0("https://api.telegram.org/bot", telegram_token, "/sendMessage"),
    body = list(chat_id = telegram_chat_id, text = message)
)

# --- 4. 최종 파일 저장 ---
write.table(new_data, "final_chatbot_knowledge.csv", sep = "\t", row.names = FALSE)
