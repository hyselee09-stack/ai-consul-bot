# update_knowledge.R
# 이 스크립트는 GitHub Actions 환경에서 실행되며, G4K 데이터를 갱신하고 텔레그램 알림을 발송합니다.

library(httr)
library(rvest)
library(dplyr)
library(stringr)

# --- 1. 환경 변수 로드 (GitHub Secrets에서 가져옴) ---
telegram_token <- Sys.getenv("TELEGRAM_BOT_TOKEN") 
telegram_chat_id <- Sys.getenv("MY_CHAT_ID") # ⭐⭐⭐ 이 부분을 "MY_CHAT_ID"로 변경 ⭐⭐⭐
g4k_url <- "https://www.g4k.go.kr/cipp/0200/selectCIPP0201.do?cffdnCd=CIPOK00049"


# --- 2. G4K 데이터 수집 및 정제 ---
print("1. G4K 데이터 수집 시작...")
tryCatch({
    res <- httr::GET(g4k_url, httr::user_agent("GHA R Scraper"))
    page <- rvest::read_html(res)

    # 본문 전체 텍스트 추출 (메뉴바 포함)
    g4k_content <- page %>% html_node("body") %>% html_text()

    # 최종 정제: 메뉴 덩어리 제거 로직 (가장 긴 정규표현식)
    final_g4k_clean <- g4k_content %>%
        str_remove("^.*?여권 정보 증명서 발급 발급 예외적 여권사용 허가 신청 신청 공증·아포스티유") %>%
        str_remove("개인정보 처리방침.*?이 누리집은 대한민국 공식 전자정부 누리집입니다\\.") %>%
        str_replace_all("신청 신청|발급 발급|조회 조회", " ") %>%
        str_squish()
    
    # 길이가 6000자 이상(즉, 내용이 존재)하면 성공으로 간주
    is_success <- nchar(final_g4k_clean) > 6000 

}, error = function(e) {
    print(paste("❌ 데이터 수집 오류 발생:", e$message))
    is_success <- FALSE
    final_g4k_clean <- "데이터 수집 중 오류가 발생하여 내용을 확보하지 못했습니다."
})


# --- 3. 데이터프레임 구성 및 저장 ---
new_data <- data.frame(
    Type = "안내문", 
    Title = "재외동포 등록 및 민원 표준 안내", 
    Content = final_g4k_clean, 
    Source = "재외동포365포털", 
    Link = g4k_url, 
    stringsAsFactors = FALSE
)
write.table(new_data, "final_chatbot_knowledge.csv", sep = "\t", row.names = FALSE)


# --- 4. 텔레그램 알림 발송 (디버깅 포함) ---
print("2. 텔레그램 알림 발송 시도...")

message <- if (is_success) {
    "✅ G4K 지식 베이스 갱신 성공. 최신 데이터로 업데이트되었습니다."
} else {
    "🚨 G4K 지식 베이스 갱신 실패. 페이지 구조를 확인하세요. 데이터가 비었거나 오류가 발생했습니다."
}

# 텔레그램 API 호출
telegram_response <- httr::POST(
    url = paste0("https://api.telegram.org/bot", telegram_token, "/sendMessage"),
    body = list(chat_id = telegram_chat_id, text = message)
)

# ⭐⭐⭐ 디버깅을 위한 API 응답 상태 출력 ⭐⭐⭐
# 이 코드가 200이 아니면 (예: 400 Bad Request) Chat ID나 Token 오류입니다.
print(paste("Telegram API Status Code:", httr::status_code(telegram_response))) 
print("--------------------------------------------------------------------")
