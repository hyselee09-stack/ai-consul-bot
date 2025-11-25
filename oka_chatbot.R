# oka_chatbot.R - Plumber API Server for OKA Chatbot

# --- 전역 설정 및 라이브러리 로드 ---
library(plumber)
library(dplyr)
library(stringr)
library(httr)

# --- 1. 지식 베이스 로드 (서버 시작 시 1회만 실행) ---
# final_chatbot_knowledge.csv 파일이 현재 작업 디렉토리에 있어야 합니다.
KNOWLEDGE_BASE <- tryCatch({
  read.csv("final_chatbot_knowledge.csv", sep = "\t", stringsAsFactors = FALSE)
}, error = function(e) {
  stop("final_chatbot_knowledge.csv 파일을 찾을 수 없습니다. 서버 구동을 중단합니다.")
})

# API 키 및 엔드포인트 설정
# R 세션에 Sys.setenv(GEMINI_API_KEY = "...") 명령어로 키가 설정되어 있어야 합니다.
GEMINI_API_KEY <- Sys.getenv("GEMINI_API_KEY") 
# Gemini API는 URL 뒤에 ?key= 가 붙어야 하므로 이 형태를 유지합니다.
MODEL_ENDPOINT <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=" 


# --- 2. 핵심 함수: 검색 및 점수 계산 (RAG Search Engine) ---
score_matches <- function(content, query) {
  keywords <- unlist(str_split(query, "[[:punct:]\\s]+"))
  score <- sum(sapply(keywords, function(k) str_detect(content, ignore.case(k))))
  return(score)
}


# -------------------------------------------------------------

# --- 3. API 엔드포인트: 실시간 질문 응답 ---
#* 챗봇의 메인 질의 응답 엔드포인트
#* @param user_query 사용자로부터 받은 질문 텍스트
#* @post /query
function(user_query) {
  
  # --- RAG 검색 (Context 추출) ---
  scores <- sapply(KNOWLEDGE_BASE$Content, score_matches, query = user_query)

  top_data <- KNOWLEDGE_BASE %>%
    mutate(score = scores) %>%
    filter(score > 0) %>%
    arrange(desc(score)) %>%
    slice_head(n = 3) # 상위 3개 문맥 정보 추출
  
  context_for_ai <- paste(top_data$Content, collapse = " | ")
  
  if (nchar(context_for_ai) < 50) {
    return("죄송합니다. 현재 OKA 지식 베이스에서 관련 정보를 찾을 수 없습니다.")
  }

  # --- AI 프롬프트 구성 및 API 호출 ---
  
  system_prompt <- "너는 재외동포청의 친절한 AI 영사야. 오직 제공된 문맥 정보만을 사용하여 한국어로 답변해줘. 만약 정보가 부족하면 '제공된 문맥으로는 답변할 수 없습니다.'라고 말해야 해."
  
  final_prompt <- paste0("문맥: ", context_for_ai, " 질문: ", user_query)
  
  # API Key가 포함된 최종 URL
  final_url <- paste0(MODEL_ENDPOINT, GEMINI_API_KEY) 
  
  # API 요청 및 AI 응답 추출
  response <- httr::POST(
    url = final_url,
    httr::content_type_json(),
    body = list(
      contents = list(
        list(role = "system", text = system_prompt),
        list(role = "user", text = final_prompt)
      )
    ),
    encode = "json"
  )

  # 3. AI 응답 추출 및 반환
  ai_answer <- tryCatch({
    httr::content(response, "parsed")$candidates[[1]]$content$parts[[1]]$text
  }, error = function(e) {
    # 500 에러 재발 방지를 위해 오류 발생 시 메시지 반환
    return(paste("API 호출 실패: 키를 Sys.setenv()로 설정했는지 확인하세요.", e$message))
  })

  return(ai_answer)
}
