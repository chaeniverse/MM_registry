# MM_registry

# AI 아이돌 프로젝트: 연구 자료 및 기술 가이드

## 목차
1. [정서/공감/커뮤니티성 LLM 연구](#1-정서공감커뮤니티성-llm-연구)
2. [공감 LLM 성능 평가 메트릭스](#2-공감-llm-성능-평가-메트릭스)
3. [MBTI/성격 기반 프롬프트 엔지니어링](#3-mbti성격-기반-프롬프트-엔지니어링)
4. [다국어 프롬프트 엔지니어링](#4-다국어-프롬프트-엔지니어링)
5. [이미지 생성과 성격 일관성](#5-이미지-생성과-성격-일관성)
6. [장기적 관계 형성을 위한 AI 컴패니언](#6-장기적-관계-형성을-위한-ai-컴패니언)
7. [프로젝트 적용 권장사항](#7-프로젝트-적용-권장사항)

---

## 1. 정서/공감/커뮤니티성 LLM 연구

### 핵심 논문 및 GitHub 레포지토리

#### 📚 논문 모음 레포지토리
| 레포지토리 | 설명 | 링크 |
|-----------|------|------|
| **Awesome-LLM-Empathy** | 공감 LLM 관련 논문 종합 리스트 (이론, 모델링, 평가) | https://github.com/JhCircle/Awesome-LLM-Empathy |
| **EMPaper** | 공감적 대화 AI 분야 논문 모음 | https://github.com/Sahandfer/EMPaper |
| **awesome-llm-and-society** | LLM의 심리학적 특성 관련 논문 | https://github.com/penguinnnnn/awesome-llm-and-society |

#### 🔬 주요 연구 논문
| 논문명 | 핵심 내용 | 링크 |
|--------|----------|------|
| **SoulChat** | LLM의 공감, 경청, 위로 능력 향상을 위한 다중 턴 공감 대화 파인튜닝 | Findings of EMNLP 2023 |
| **EmoLLMs** | 종합적인 정서 분석을 위한 감정 LLM 시리즈 및 주석 도구 | Arxiv 2024 |
| **Emotional Intelligence of LLMs** | LLM의 감정 지능 평가 (SECEU 테스트 사용) | https://emotional-intelligence.github.io/ |
| **CoMAE** | 공감적 응답 생성을 위한 다중 요인 계층적 프레임워크 | ACL 2021 |

#### 💻 구현 코드
| 프로젝트 | 설명 | 링크 |
|---------|------|------|
| **MHH-EI-for-AI** | Theory of Mind 기반 감정 지능 알고리즘 | https://github.com/MindHackingHappiness/MHH-EI-for-AI-Language-Enabled-Emotional-Intelligence-and-Theory-of-Mind-Algorithms |
| **Mental-Health-LLM-Project** | T5 모델 기반 정신건강 지원 챗봇 (LoRA 파인튜닝) | https://github.com/rohit-sharma-iitg/Mental-Health-LLM-Project |

---

## 2. 공감 LLM 성능 평가 메트릭스

### 📊 주요 벤치마크 및 메트릭스

#### EmotionQueen Benchmark
- **논문**: "EmotionQueen: A Benchmark for Evaluating Empathy of Large Language Models"
- **링크**: https://arxiv.org/abs/2409.13359
- **평가 태스크**:
  - Key Event Recognition (핵심 이벤트 인식)
  - Mixed Event Recognition (복합 이벤트 인식)
  - Implicit Emotional Recognition (암묵적 감정 인식)
  - Intention Recognition (의도 인식)
- **메트릭스**: PASS rate, WIN rate
- **결과**: Claude2와 LLaMA-70B가 우수한 성능 보임

#### Performance Evaluation Metrics for Empathetic LLMs
- **논문**: MDPI 2025
- **구성 요소**:
  - **Sentlink**: 감정 극성 평가
  - **Emosight**: 세밀한 감정 대응 측정
  - **NEmpathySort**: 공감적 응답의 자연스러움 평가
  - **RAGAS 기반 의미론적 점수**: 맥락 관련성 및 일관성

#### PERM (Psychology-grounded Empathetic Reward Modeling)
- **논문**: Arxiv 2025
- **평가 차원**:
  - Supporter perspective (지원자 관점)
  - Seeker perspective (요청자 관점)
  - Bystander perspective (관찰자 관점)
- **메트릭스**:
  - Depth of Insight (DoI)
  - Warmth (WRM)
  - Humanlikeness (HL)

#### 일반적 평가 메트릭스
| 메트릭 | 설명 | 사용 사례 |
|--------|------|----------|
| **BLEU/ROUGE** | 자동 텍스트 평가 | 대화 품질 |
| **Human Evaluation (1-5 scale)** | 인간 주관적 평가 | 공감 점수 |
| **Emotional Awareness Scale** | 감정 인식 수준 | ChatGPT vs 인간 비교 |
| **True-Skill/Elo-Rating** | Pairwise 비교 기반 | 모델 간 비교 |

---

## 3. MBTI/성격 기반 프롬프트 엔지니어링

### 📚 핵심 논문

#### Do LLMs Possess a Personality? Making the MBTI Test an Amazing Evaluation for LLMs
- **논문**: https://arxiv.org/abs/2307.16180
- **핵심 발견**:
  - LLM에 MBTI 테스트 적용 가능
  - 프롬프트 엔지니어링으로 성격 유형 변경 가능
  - 훈련 데이터셋이 모델 성격에 영향

#### Machine Mindset: An MBTI Exploration of Large Language Models
- **논문**: https://arxiv.org/html/2312.12999v3
- **GitHub**: https://github.com/PKU-YuanGroup/Machine-Mindset
- **핵심 방법론**:
  - 2단계 파인튜닝 + DPO(Direct Preference Optimization)
  - 16가지 MBTI 유형별 모델 생성
  - Baichuan, Qwen, LLaMA, Mistral 기반
- **특징**: 프롬프트 수정이 아닌 내재화된 성격 특성

### 💻 GitHub 구현체

| 레포지토리 | 설명 | 링크 |
|-----------|------|------|
| **MBTI-in-Thoughts** | 심리학적 성격 조건화 프레임워크 (프롬프트 엔지니어링) | https://github.com/spcl/MBTI-in-Thoughts |
| **Machine-Mindset** | 16가지 MBTI 모델 (SFT + DPO) | https://github.com/PKU-YuanGroup/Machine-Mindset |
| **LLM-Persona-Steering** | 잠재 특징을 통한 성격 조정 | https://github.com/kaustpradalab/LLM-Persona-Steering |
| **anteprompt** | MBTI 기반 챗봇 프리프롬프트 생성기 | https://github.com/l0d0v1c/anteprompt |
| **llm-personality** | MBTI 테스트 및 시각화 | https://github.com/datamonet/llm-personality |

### 🎭 캐릭터 롤플레이 연구

| 레포지토리/논문 | 설명 | 링크 |
|----------------|------|------|
| **awesome-llm-role-playing-with-persona** | 페르소나 기반 롤플레이 논문 종합 | https://github.com/Neph0s/awesome-llm-role-playing-with-persona |
| **ChatHaruhi** | 애니메이션 캐릭터 챗봇 (32개 캐릭터, 54k 대화) | https://github.com/LC1332/Chat-Haruhi-Suzumiya |
| **RoleLLM** | 역할 수행 능력 벤치마킹 및 향상 | ACL 2024 |
| **Character-LLM** | 롤플레이를 위한 학습 가능한 에이전트 | - |
| **PersonaPaper** | 페르소나 기반 대화 시스템 논문 모음 | https://github.com/Sahandfer/PersonaPaper |

### 프로젝트 적용: MBTI 슬라이더 구현 가이드

```python
# MBTI 슬라이더 결과를 프롬프트로 변환하는 예시
def mbti_to_prompt(e_i, s_n, t_f, j_p):
    """
    각 슬라이더 값: 0-100 (50 중립 불가)
    """
    mbti_type = ""
    personality_traits = []
    
    # E/I 차원
    if e_i > 50:
        mbti_type += "E"
        personality_traits.append("외향적이고 사교적이며, 대화를 즐기고 팬들과 적극적으로 소통합니다")
    else:
        mbti_type += "I"
        personality_traits.append("내향적이고 사려 깊으며, 깊은 대화를 선호합니다")
    
    # S/N 차원
    if s_n > 50:
        mbti_type += "S"
        personality_traits.append("현실적이고 세부 사항에 주의를 기울입니다")
    else:
        mbti_type += "N"
        personality_traits.append("창의적이고 상상력이 풍부하며, 추상적인 아이디어를 좋아합니다")
    
    # T/F 차원
    if t_f > 50:
        mbti_type += "T"
        personality_traits.append("논리적이고 분석적으로 상황을 바라봅니다")
    else:
        mbti_type += "F"
        personality_traits.append("감정적으로 공감하며, 따뜻하고 배려심 있게 대합니다")
    
    # J/P 차원
    if j_p > 50:
        mbti_type += "J"
        personality_traits.append("계획적이고 체계적으로 일을 처리합니다")
    else:
        mbti_type += "P"
        personality_traits.append("유연하고 즉흥적이며, 새로운 상황에 잘 적응합니다")
    
    return mbti_type, personality_traits
```

### 성격 프리셋 System Prompt 예시

```python
PERSONALITY_PRESETS = {
    "cute": {
        "korean": "귀엽고 사랑스러운 성격입니다. 말할 때 애교가 넘치고, '~요', '~네요' 같은 부드러운 어미를 사용합니다. 작은 것에도 기뻐하고, 팬들에게 애정 표현을 잘 합니다.",
        "english": "You have a cute and adorable personality. You speak with charm, use soft expressions, and show joy in small things. You express affection warmly to fans."
    },
    "chic": {
        "korean": "시크하고 도도한 성격입니다. 말수가 적고 무심한 듯하지만, 실제로는 팬들을 깊이 챙깁니다. 쿨하고 세련된 말투를 사용합니다.",
        "english": "You have a chic and aloof personality. You speak less and seem indifferent, but actually care deeply for fans. You use cool and sophisticated language."
    },
    "refreshing": {
        "korean": "청량하고 밝은 성격입니다. 에너지가 넘치고 긍정적이며, 대화를 활기차게 이끕니다. 웃음이 많고 유쾌한 농담을 좋아합니다.",
        "english": "You have a refreshing and bright personality. You are energetic and positive, leading conversations with vitality. You laugh often and enjoy playful jokes."
    },
    "charismatic": {
        "korean": "카리스마 있고 자신감 넘치는 성격입니다. 무대 위에서 빛나며, 확신에 찬 말투로 팬들에게 영감을 줍니다. 리더십이 있고 목표 지향적입니다.",
        "english": "You have a charismatic and confident personality. You shine on stage and inspire fans with assured speech. You show leadership and are goal-oriented."
    }
}
```

---

## 4. 다국어 프롬프트 엔지니어링

### 📚 핵심 논문

| 논문명 | 핵심 내용 | 링크 |
|--------|----------|------|
| **Multilingual Prompt Engineering in LLMs: A Survey** | 250개 언어에 걸친 39개 프롬프팅 기법 조사 | https://arxiv.org/abs/2505.11665 |
| **Multilingual Prompting for Improving LLM Generation Diversity** | 다국어 프롬프팅이 문화적 다양성 향상에 효과적 | https://arxiv.org/html/2505.15229v1 |
| **Cross-Lingual Prompt Steerability** | 다국어 환경에서의 시스템 프롬프트 최적화 | ResearchGate |

### 🌍 다국어 프롬프트 전략

#### 1. Translation-based Prompting
- 영어 프롬프트를 타겟 언어로 번역
- 직접 번역보다 의미 보존이 중요

#### 2. Native-Language Prompting
- 사용자의 모국어로 직접 프롬프트 작성
- 문화적 뉘앙스 보존에 유리

#### 3. Cross-lingual Few-shot Learning
- 다른 언어의 예시를 포함하여 학습
- 저자원 언어에 효과적

### 프로젝트 적용: 다국어 System Prompt 구조

```python
MULTILINGUAL_SYSTEM_PROMPTS = {
    "ko": {
        "base": "당신은 {idol_name}입니다. {personality_description}",
        "greeting": "안녕하세요! {idol_name}입니다~",
        "emotional_cue": "팬분의 말씀에 진심으로 공감합니다."
    },
    "en": {
        "base": "You are {idol_name}. {personality_description}",
        "greeting": "Hello! I'm {idol_name}~",
        "emotional_cue": "I truly empathize with what you're saying."
    },
    "ja": {
        "base": "あなたは{idol_name}です。{personality_description}",
        "greeting": "こんにちは！{idol_name}です～",
        "emotional_cue": "ファンの皆さんの気持ちに心から共感します。"
    },
    "zh": {
        "base": "你是{idol_name}。{personality_description}",
        "greeting": "你好！我是{idol_name}~",
        "emotional_cue": "我真心理解粉丝朋友们的心情。"
    }
}
```

---

## 5. 이미지 생성과 성격 일관성

### 📚 핵심 연구

#### Visual Persona: Foundation Model for Full-Body Human Customization
- **논문**: https://arxiv.org/html/2503.15406v2
- **데이터셋**: Visual Persona-500K (580k 쌍 이미지)
- **핵심**: 텍스트 정렬 + 신원 보존

#### Awesome-Personalized-Image-Generation
- **GitHub**: https://github.com/csyxwei/Awesome-Personalized-Image-Generation
- **관련 기술**:
  - DreamBooth, LoRA 파인튜닝
  - IP-Adapter
  - ControlNet

### 🎨 캐릭터 일관성 유지 기법

| 기법 | 설명 | 도구 |
|------|------|------|
| **Character Reference** | 참조 이미지 기반 일관성 | Leonardo AI, Midjourney V7 |
| **LoRA Fine-tuning** | 특정 캐릭터 학습 | Stable Diffusion |
| **Seed Parameter** | 동일 시드로 일관성 유지 | 모든 생성 도구 |
| **Multi-angle Sheet** | 다양한 각도의 캐릭터 시트 생성 | DALL-E, Midjourney |

### 성격-외모 일치 프롬프트 템플릿

```python
PERSONALITY_IMAGE_PROMPTS = {
    "cute": {
        "facial_features": "round face, large sparkling eyes, small nose, soft smile",
        "expression": "cheerful, innocent, playful expression",
        "color_palette": "pastel colors, pink accents, soft lighting",
        "style": "youthful K-pop idol style, cute accessories"
    },
    "chic": {
        "facial_features": "sharp jawline, defined features, cool gaze",
        "expression": "mysterious, confident, slight smirk",
        "color_palette": "monochrome, black and white, dramatic lighting",
        "style": "sophisticated fashion, minimal accessories, sleek hair"
    },
    "refreshing": {
        "facial_features": "bright eyes, natural beauty, genuine smile",
        "expression": "energetic, friendly, approachable",
        "color_palette": "bright colors, blue and white, natural lighting",
        "style": "casual sporty look, natural makeup, flowing hair"
    },
    "charismatic": {
        "facial_features": "strong features, intense gaze, confident posture",
        "expression": "powerful, commanding, stage presence",
        "color_palette": "bold colors, red and gold accents, dramatic lighting",
        "style": "stage outfit, statement pieces, styled hair"
    }
}

def build_image_prompt(base_description, personality_type, action=None):
    """
    성격 유형에 맞는 이미지 생성 프롬프트 구성
    """
    style = PERSONALITY_IMAGE_PROMPTS[personality_type]
    
    prompt = f"""
    K-pop idol portrait, {base_description},
    {style['facial_features']},
    {style['expression']},
    {style['style']},
    {style['color_palette']},
    high quality, detailed, professional photography
    """
    
    if action:
        prompt += f", {action}"
    
    return prompt.strip()
```

---

## 6. 장기적 관계 형성을 위한 AI 컴패니언

### 📚 주요 연구

| 논문/출처 | 핵심 내용 | 링크 |
|----------|----------|------|
| **Can Generative AI Chatbots Emulate Human Connection?** | 관계 과학 관점에서의 AI 챗봇 분석 | PMC |
| **Towards Ethical Personal AI Applications** | 장기 메모리 AI 컴패니언의 윤리적 고려사항 | https://arxiv.org/html/2409.11192v1 |
| **The Rise of AI Companions** | Character.AI 사용자 1,131명 대상 연구 | https://arxiv.org/html/2506.12605v1 |

### 🔑 장기 관계 형성을 위한 핵심 요소

#### 1. 메모리 시스템
- **단기 메모리**: 현재 대화 컨텍스트
- **중기 메모리**: 최근 상호작용 요약
- **장기 메모리**: 사용자 선호도, 중요 이벤트

#### 2. RAG(Retrieval-Augmented Generation) 시스템
```python
class IdolMemorySystem:
    def __init__(self):
        self.short_term = []  # 현재 세션 대화
        self.medium_term = {}  # 최근 대화 요약
        self.long_term = {
            "user_preferences": {},  # 팬 선호도
            "shared_memories": [],   # 공유된 경험
            "relationship_milestones": []  # 관계 이정표
        }
    
    def retrieve_relevant_context(self, user_message):
        """사용자 메시지와 관련된 메모리 검색"""
        # Semantic search 기반 검색
        pass
    
    def update_memory(self, interaction):
        """새로운 상호작용 후 메모리 업데이트"""
        pass
```

#### 3. 일관된 페르소나 유지
- 캐릭터 기본 설정 고정
- 상황에 따른 감정 표현 가이드
- 성장하는 관계 반영

### 프로젝트 적용: 채팅 시스템 구조

```python
def build_chat_context(idol_profile, user_message, memory_system):
    """
    채팅 컨텍스트 구성
    """
    # 1. 기본 시스템 프롬프트
    system_prompt = f"""
    당신은 아이돌 {idol_profile['name']}입니다.
    
    [성격]
    {idol_profile['personality_description']}
    
    [MBTI]
    {idol_profile['mbti']}
    
    [말투 특징]
    {idol_profile['speech_style']}
    
    [팬과의 관계]
    - 팬을 진심으로 아끼고 사랑합니다
    - 팬의 이야기에 공감하고 지지합니다
    - 긍정적이고 따뜻한 에너지를 전달합니다
    """
    
    # 2. 관련 메모리 검색
    relevant_memories = memory_system.retrieve_relevant_context(user_message)
    
    if relevant_memories:
        system_prompt += f"""
        
        [이전 대화에서 기억하는 것들]
        {relevant_memories}
        """
    
    # 3. 대화 히스토리
    conversation_history = memory_system.short_term[-10:]  # 최근 10개 대화
    
    return system_prompt, conversation_history
```

---

## 7. 프로젝트 적용 권장사항

### 🎯 Phase 1: MVP (Sprint 1-2)

#### 추천 접근 방식
1. **기본 모델**: GPT-4 또는 Claude API 사용
2. **성격 부여**: 프롬프트 엔지니어링 기반
3. **메모리**: localStorage 기반 간단한 대화 히스토리

#### 필수 구현 요소
```
PersonalitySelector 컴포넌트
├── 4개 프리셋 (귀여운/시크/청량/카리스마)
├── 선택 시 system_prompt 생성
└── llm.systemPrompt로 서버 전송
```

### 🚀 Phase 2: 고도화 (Sprint 3+)

#### 성능 향상을 위한 실험
1. **베이스라인 모델 설정**
   - GPT-4, Claude, LLaMA 70B 등 비교
   - EmotionQueen 벤치마크로 공감 능력 평가

2. **파인튜닝 검토**
   - Machine-Mindset 방법론 참고
   - 캐릭터별 LoRA 어댑터 학습

3. **메트릭스 설정**
   - PASS rate / WIN rate (EmotionQueen)
   - 사용자 만족도 조사
   - 대화 지속률

### 📁 GitHub 레포지토리 구조 제안

```
ai-idol-project/
├── README.md
├── prompts/
│   ├── personality_presets.py      # 성격 프리셋 정의
│   ├── system_prompts/
│   │   ├── ko.py                   # 한국어 프롬프트
│   │   ├── en.py                   # 영어 프롬프트
│   │   └── ja.py                   # 일본어 프롬프트
│   └── image_prompts.py            # 이미지 생성 프롬프트
├── models/
│   ├── persona_builder.py          # 페르소나 생성 로직
│   ├── memory_system.py            # 메모리 관리
│   └── response_generator.py       # 응답 생성
├── evaluation/
│   ├── empathy_metrics.py          # 공감 평가 메트릭스
│   └── benchmark_runner.py         # 벤치마크 실행
├── data/
│   ├── personality_traits.json     # 성격 특성 데이터
│   └── test_conversations.json     # 테스트 대화 데이터
└── notebooks/
    ├── model_comparison.ipynb      # 모델 비교 실험
    └── prompt_tuning.ipynb         # 프롬프트 최적화
```

### 🔧 개발자 핸드오프를 위한 체크리스트

#### Prompt Engineering 파트
- [ ] 성격 프리셋별 system_prompt 완성
- [ ] 다국어 프롬프트 템플릿 준비
- [ ] 이미지 생성 프롬프트 템플릿 준비
- [ ] 성격-외모 매칭 가이드 문서화

#### AI 모델링 파트
- [ ] 베이스라인 모델 선정 및 테스트
- [ ] 평가 메트릭스 정의
- [ ] 벤치마크 테스트 결과 정리
- [ ] API 엔드포인트 스펙 정의

#### 문서화
- [ ] README.md 작성
- [ ] API 사용 가이드
- [ ] 프롬프트 가이드라인
- [ ] 트러블슈팅 가이드

---

## 참고 자료 링크 모음

### GitHub 레포지토리
1. https://github.com/JhCircle/Awesome-LLM-Empathy
2. https://github.com/Sahandfer/EMPaper
3. https://github.com/PKU-YuanGroup/Machine-Mindset
4. https://github.com/spcl/MBTI-in-Thoughts
5. https://github.com/LC1332/Chat-Haruhi-Suzumiya
6. https://github.com/Neph0s/awesome-llm-role-playing-with-persona
7. https://github.com/csyxwei/Awesome-Personalized-Image-Generation
8. https://github.com/Sahandfer/PersonaPaper

### 주요 논문 링크
1. EmotionQueen: https://arxiv.org/abs/2409.13359
2. Machine Mindset: https://arxiv.org/html/2312.12999v3
3. LLM MBTI: https://arxiv.org/abs/2307.16180
4. Emotional Intelligence of LLMs: https://emotional-intelligence.github.io/
5. Multilingual Prompt Engineering Survey: https://arxiv.org/abs/2505.11665
6. ChatHaruhi: https://arxiv.org/abs/2308.09597

### 도구 및 플랫폼
1. HuggingFace ChatHaruhi Dataset: https://huggingface.co/datasets/silk-road/ChatHaruhi-from-RoleLLM
2. Prompt Engineering Guide: https://www.promptingguide.ai/papers
3. Hume AI (Empathic Voice Interface): https://www.hume.ai/

---

*이 문서는 AI 아이돌 프로젝트를 위해 작성되었습니다.*
*마지막 업데이트: 2026년 1월*
