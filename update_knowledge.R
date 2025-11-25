# update_knowledge.R
# 이 스크립트를 GitHub 저장소에 저장해야 합니다.

library(httr)
library(rvest)
library(dplyr)
library(stringr)

# --- 1. G4K 데이터 수집 (최신 데이터 획득) ---
# 이전에 성공했던 G4K 접속 코드를 재사용합니다. 
# (복잡한 인증/헤더 문제 없이 접속이 가능함을 확인했음)
g4k_url <- "https://www.g4k.go.kr/cipp/0200/selectCIPP0201.do?cffdnCd=CIPOK00049"
res <- GET(g4k_url, user_agent("Custom Scraper"))
page <- read_html(res)

# 2. 본문 추출 및 정제 (이전에 성공했던 최종 정제 로직)
# (중요: 이 과정은 실제 서비스에서는 오류 처리 로직이 추가되어야 합니다.)
final_g4k_clean <- page %>% html_node("body") %>% html_text() %>% 
    str_remove("^.*?여권 정보 증명서 발급 발급 예외적 여권사용 허가 신청 신청 공증·아포스티유") %>%
    str_remove("개인정보 처리방침.*?이 누리집은 대한민국 공식 전자정부 누리집입니다\\.") %>%
    str_replace_all("신청 신청|발급 발급|조회 조회", " ") %>%
    str_squish()

# 3. 데이터프레임 구성 (LA 데이터는 0건으로 고정)
new_data <- data.frame(
    Type = "안내문", 
    Title = "재외동포 등록 및 민원 표준 안내", 
    Content = final_g4k_clean, 
    Source = "재외동포365포털", 
    Link = g4k_url, 
    stringsAsFactors = FALSE
)

# 4. 텔레그램 알림 로직 추가 (선택 사항: 데이터가 변경되었을 때만 알림)
# *실제 구현에서는 파일 해시(Hash)를 비교하여 변경되었을 때만 알림을 보내야 합니다.*
telegram_token <- Sys.getenv("TELEGRAM_BOT_TOKEN") 
telegram_chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")

if (nchar(final_g4k_clean) > 6000) { # 길이가 6000자 이상(즉, 내용이 존재)하면 성공으로 간주
    message <- "✅ G4K 지식 베이스 갱신 성공. 최신 데이터로 업데이트되었습니다."
} else {
    message <- "🚨 G4K 지식 베이스 갱신 실패. 페이지 구조를 확인하세요."
}

# httr::POST(
#     url = paste0("https://api.telegram.org/bot", telegram_token, "/sendMessage"),
#     body = list(chat_id = telegram_chat_id, text = message)
# )

# 5. 최종 파일 저장 (GitHub 저장소에 갱신)
write.table(new_data, "final_chatbot_knowledge.csv", sep = "\t", row.names = FALSE)
