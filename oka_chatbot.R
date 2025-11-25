# oka_chatbot.R

# --- 필수 라이브러리 ---
library(plumber)
library(dplyr)
library(stringr)
library(httr)

# --- 1. 전역 설정: 지식 베이스 로드 ---
# 서버 시작 시 단 한번만 실행되어 메모리에 적재
# 최종적으로 통합된 CSV 파일 이름을 사용합니다.
KNOWLEDGE_BASE <- read.csv("final_chatbot_knowledge.csv", sep = "\t", stringsAsFactors = FALSE)
GEMINI_API_KEY <- Sys.getenv("GEMINI_API_KEY") # 환경 변수에서 API 키를 로드

# -------------------------------------------------------------

# --- 2. 핵심 함수: 검색 및 점수 계산 (RAG Search Engine) ---
score_matches <- function(content, query) {
  # 질문에 있는 핵심 단어들을 분리
  keywords <- unlist(str_split(query, "[[:punct:]\\s]+"))
  # Content에 키워드가 몇 개 포함되었는지 계산
  score <- sum(sapply(keywords, function(k) str_detect(content, ignore.case(k))))
  return(score)
}



#* 챗봇의 메인 질의 응답 엔드포인트
#* @param user_query 사용자로부터 받은 질문 텍스트
#* @post /query
function(user_query) {
  
  # --- 1. RAG 검색 (Context 추출) ---
  scores <- sapply(KNOWLEDGE_BASE$Content, score_matches, query = user_query)

  top_data <- KNOWLEDGE_BASE %>%
    mutate(score = scores) %>%
    filter(score > 0) %>%
    arrange(desc(score)) %>%
    slice_head(n = 3) # 상위 3개 문맥 정보 추출
  
  context_for_ai <- paste(top_data$Content, collapse = " | ")
  
  if (nchar(context_for_ai) < 50) {
    # 검색된 정보가 부족할 경우 (RAG 실패)
    return("죄송합니다. 현재 OKA 지식 베이스에서 관련 정보를 찾을 수 없습니다.")
  }

  # --- 2. AI 프롬프트 구성 및 API 호출 ---
  
  # 시스템 역할 및 규칙 지정
  system_prompt <- "너는 재외동포청의 친절한 AI 영사야. 오직 제공된 문맥 정보만을 사용하여 한국어로 답변해줘. 만약 정보가 부족하면 '제공된 문맥으로는 답변할 수 없습니다.'라고 말해야 해."
  
  final_prompt <- paste0(
    "문맥: ", context_for_ai, 
    " 질문: ", user_query
  )
  
  # Gemini API 엔드포인트 (실제 사용하시는 API 엔드포인트로 변경 필요)
  model_endpoint <- "여기에_Gemini_또는_GPT_API_엔드포인트를_입력"

  # API 요청 및 AI 응답 추출
  response <- httr::POST(
    url = model_endpoint,
    httr::add_headers(Authorization = paste("Bearer", GEMINI_API_KEY)),
    httr::content_type_json(),
    body = list(
      contents = list(
        list(role = "system", text = system_prompt),
        list(role = "user", text = final_prompt)
      )
    ),
    encode = "json"
  )

  # 응답 텍스트 추출 (API 응답 구조에 맞게 수정 필요)
  ai_answer <- tryCatch({
    # 추출 로직은 사용하시는 API의 JSON 구조에 따라 달라집니다.
    httr::content(response, "parsed")$candidates[[1]]$content$parts[[1]]$text
  }, error = function(e) {
    return("API 응답 처리 중 오류가 발생했습니다.")
  })

  return(ai_answer)
}

# --- 서버 실행 명령어 예시 ---
# # R 콘솔에서 아래 명령 실행:
# # library(plumber); pr("oka_chatbot.R")$run(port=8000)
