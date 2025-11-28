# 필요한 라이브러리 로드
library(plumber)
library(dplyr)
library(stringr)
library(httr)

# --- 0. 전역 변수 및 초기화 ---

# API 키 및 엔드포인트 설정 (GitHub Secrets에서 키를 안전하게 읽어옴)
GEMINI_API_KEY <- Sys.getenv("GEMINI_API_KEY") 
MODEL_ENDPOINT <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key="

# KNOWLEDGE_BASE 로드 (파일은 같은 디렉토리에 있어야 함)
KNOWLEDGE_BASE <- read.csv("final_chatbot_knowledge.csv", stringsAsFactors = FALSE)


# --- 1. 핵심 함수: 검색 및 점수 계산 (RAG Search Engine) ---
score_matches <- function(content, query) {
  # 1. 키워드 분리
  keywords <- unlist(stringr::str_split(query, "[[:punct:]\\s]+"))
  
  # 2. 빈 문자열("") 키워드를 제거합니다. (최종 RAG 버그 수정)
  keywords <- keywords[keywords != ""] 
  
  if (length(keywords) == 0) return(0)
  
  # 3. Score 계산 (stringr::fixed(ignore_case = TRUE) 사용으로 충돌 방지)
  score <- sum(sapply(keywords, function(k) stringr::str_detect(content, stringr::fixed(k, ignore_case = TRUE))))
  return(score)
}


# --- 2. 챗봇 엔드포인트 ---

#* 챗봇의 메인 질의 응답 엔드포인트
#* @param user_query 사용자로부터 받은 질문 텍스트
#* @post /query
function(user_query) {
  
  # ⭐ API Key 로드 (함수 내부에서 환경 변수 재확인)
  local_gemini_key <- Sys.getenv("GEMINI_API_KEY") 
  
  if (is.na(local_gemini_key) || nchar(local_gemini_key) < 30) {
    # 이 메시지는 로컬 환경에서만 출력되며, GitHub에서는 Secret이 로드되므로 통과합니다.
    return("500 ERROR: API Key (GEMINI_API_KEY)가 설정되지 않았거나 길이가 너무 짧습니다. Sys.setenv() 명령을 확인하세요.")
  }

  # --- 1. RAG 검색 (Context 추출) ---
  scores <- sapply(KNOWLEDGE_BASE$Content, score_matches, query = user_query)
  top_data <- KNOWLEDGE_BASE %>%
    mutate(score = scores) %>%
    filter(score > 0) %>%
    arrange(desc(score)) %>%
    slice_head(n = 3)
  
  context_for_ai <- paste(top_data$Content, collapse = " | ")
  
  if (nchar(context_for_ai) < 50) {
    return("죄송합니다. 현재 OKA 지식 베이스에서 관련 정보를 찾을 수 없습니다.")
  }

  # --- 2. AI 프롬프트 구성 및 API 호출 ---
  system_prompt <- "너는 재외동포청의 친절한 AI 영사야. 오직 제공된 문맥 정보만을 사용하여 한국어로 답변해줘. 만약 정보가 부족하면 '제공된 문맥으로는 답변할 수 없습니다.'라고 말해야 해."
  final_prompt <- paste0("문맥: ", context_for_ai, " 질문: ", user_query)
  
  # 최종 URL 구성
  final_url <- paste0(MODEL_ENDPOINT, local_gemini_key) 
  
  
  ai_answer <- tryCatch({
    
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

    # HTTP 상태 코드 확인 (400, 401 오류 시 즉시 중단)
    if (httr::status_code(response) != 200) {
        stop(paste("API Error Status:", httr::status_code(response), "Details:", httr::content(response, "text")))
    }

    # 정상 응답 텍스트 추출
    text_content <- httr::content(response, "parsed")$candidates[[1]]$content$parts[[1]]$text

    if (is.null(text_content)) {
        stop("AI가 텍스트를 생성하지 못했거나 응답 구조가 비어 있습니다.")
    }
    
    return(text_content)
    
  }, error = function(e) {
    # 모든 오류를 잡고 명확한 메시지를 반환
    return(paste("최종 오류: API 호출 중단. 원인:", e$message))
  })

  return(ai_answer)
}
