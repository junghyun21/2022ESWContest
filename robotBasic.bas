'******** 기본 셋팅 값 ********
CONST 고개각도_라인트레이싱상수 = 24
CONST 고개각도_방위인식상수= 98
CONST 고개각도_방향인식상수 = 107
CONST 고개각도_방이름인식상수 = 100
CONST 고개각도_탈출상수 = 30 ' 네번째, 다섯번째 라인트레이싱 시 고개 각도
CONST 미션방_횡이동입장상수 = 15
CONST 미션방_종이동탈출상수 = 2
CONST 미션방_종확인 = 105
CONST 계단_인식상수 = 20
CONST 계단_오르기상수 = 100
CONST 위험_시민인식상수 = 80
CONST 위험_횡이동횟수상수 = 2
CONST 계단_초록색전진인식상수= 6
CONST 고개각도_초록색= 75
CONST 고개각도_좌우방위 = 10


'******** 송수신 데이터 ********

CONST LETGO = 0

CONST Arrow_detect = 1
CONST Arrow_no = 10
CONST Arrow_left = 11
CONST Arrow_right = 12

CONST Alpha_detect = 2
CONST Alpha_no= 20
CONST Alpha_E = 21
CONST Alpha_W = 22
CONST Alpha_S = 23
CONST Alpha_N = 24
CONST Alpha_A = 25
CONST Alpha_C = 26
CONST Alpha_B = 27
CONST Alpha_D = 28
CONST Alpha_NWES = 29

CONST Line_detect = 3
CONST Line_no = 30
CONST Line_mid = 31
CONST Line_left = 32
CONST Line_right = 33
CONST Line_threeway_no = 34
CONST Line_threeway_yes = 35
CONST Line_angle_mid = 36
CONST Line_angle_left = 37
CONST Line_angle_right = 38
CONST Three_way_detect = 39

CONST T_detect = 130
CONST T_no = 131
CONST T_yes = 132

CONST Blue_detect = 4
CONST Blue_no = 40
CONST Blue_yes = 41

CONST Yellow_detect = 5
CONST Yellow_no = 50
CONST Yellow_yes = 51

CONST Black_detect = 6
CONST Black_no = 60
CONST Black_yes = 61

CONST Green_detect = 7
CONST Green_no = 70
CONST Green_yes = 71

CONST Red_detect = 8
CONST Red_no = 80
CONST Red_yes = 81

CONST BluePerson_detect = 9
CONST BluePerson_detect_no = 90
CONST BluePerson_detect_mid = 91
CONST BluePerson_detect_left = 92
CONST BluePerson_detect_right = 93
CONST BluePerson_detect_grap = 94

CONST Bell_detect = 100

'******** 기타 데이터 ********

CONST TRUE = 1
CONST FALSE = 0

DIM S AS BYTE ' 데이터 수신
DIM DIRECTION AS BYTE ' 방향 저장
DIM ROOM AS BYTE ' 방이름 저장
DIM DANGEROUS_ROOM AS BYTE ' 위험지역 미션방 이름 저장
DIM LINE_TRACING_NUMBER AS BYTE ' 현재 라인트레이싱을 실행하고 있는 지점
DIM UPDOWN AS BYTE ' 고개 상하 동작 각도
DIM IS_PERSON AS BYTE ' 시민 인식 여부
DIM STAIRS_CNT AS BYTE ' 현재 오른 계단의 층 수 저장
DIM GREEN_WALK_CNT AS BYTE ' 초록색 찾으려고 전진한 횟수
DIM LETTER AS BYTE
DIM ARROW AS BYTE
DIM IS_T AS BYTE

STAIRS_CNT = 0
IS_PERSON = FALSE
LINE_TRACING_NUMBER = 1
GREEN_WALK_CNT = 0
ARROW = 100
IS_T = FALSE

'******** 2족 보행로봇 초기 영점 프로그램 ********

DIM A AS BYTE
DIM A_old AS BYTE
DIM B AS BYTE
DIM I AS BYTE
DIM MODE AS BYTE
DIM 넘어진확인 AS BYTE
DIM 기울기확인횟수 AS BYTE
DIM 적외선거리값  AS BYTE
DIM 보행순서 AS BYTE
DIM 보행횟수 AS BYTE
DIM 보행COUNT AS BYTE
DIM 반전체크 AS BYTE
DIM 모터ONOFF AS BYTE
DIM 자이로ONOFF AS BYTE
DIM 보행속도 AS BYTE
DIM 좌우속도 AS BYTE

DIM NO_0 AS BYTE
DIM NO_1 AS BYTE
DIM NO_2 AS BYTE
DIM NO_3 AS BYTE
DIM NO_4 AS BYTE
DIM NUM AS BYTE

DIM S11 AS BYTE
DIM S16 AS BYTE

DIM BUTTON_NO AS INTEGER
DIM SOUND_BUSY AS BYTE
DIM TEMP_INTEGER AS INTEGER

'**** 기울기센서포트 설정 ****

CONST 앞뒤기울기AD포트 = 0
CONST 좌우기울기AD포트 = 1
CONST 적외선AD포트  = 4

CONST 기울기확인시간 = 20  'ms

CONST min = 61	'뒤로넘어졌을때
CONST max = 107	'앞으로넘어졌을때
CONST COUNT_MAX = 3

CONST 머리이동속도 = 10

'************************************************

PTP SETON 				'단위그룹별 점대점동작 설정
PTP ALLON				'전체모터 점대점 동작 설정

DIR G6A,1,0,0,1,0,0		'모터0~5번
DIR G6D,0,1,1,0,1,1		'모터18~23번
DIR G6B,1,1,1,1,1,1		'모터6~11번
DIR G6C,0,0,0,0,1,0		'모터12~17번

'************************************************

OUT 52,0	'머리 LED 켜기

'***** 초기선언 '************************************************

보행순서 = 0
반전체크 = 0
기울기확인횟수 = 0
보행횟수 = 1
모터ONOFF = 0

'****초기위치 피드백*****************************

TEMPO 230
MUSIC "cdefg"

SPEED 5
GOSUB MOTOR_ON

S11 = MOTORIN(11)
S16 = MOTORIN(16)

SERVO 11, 100
SERVO 16, S16

SERVO 16, 100

GOSUB 전원초기자세
GOSUB 기본자세

GOSUB 자이로INIT
GOSUB 자이로MID
GOSUB 자이로ON

' PRINT "VOLUME 200 !"
' PRINT "SOUND 12 !" '안녕하세요

GOSUB All_motor_mode3

GOTO MAIN_0	'시리얼 수신 루틴으로 가기

'*************************************************************************
'*************************************************************************
'************************** 제어보드 알고리즘 부분 *****************************
'*************************************************************************
'*************************************************************************

MAIN_0:
    ERX 4800,A,MAIN_0 ' 로봇 연결 될 때까지 대기

    IF A = LETGO THEN ' 라즈베리파이로부터 0 수신 시, 코드 시작
        UPDOWN = 고개각도_방위인식상수
    	GOSUB 고개동작

    	GOTO 방위인식
    ELSE
        GOTO MAIN_0
    ENDIF
    

'***************************************************

MAIN:
    GOSUB 앞뒤기울기측정
    GOSUB 좌우기울기측정
    GOSUB 적외선거리센서확인
	
    IF 넘어진확인 = 1 THEN 
        넘어진확인 = 0
    ENDIF

    IF LINE_TRACING_NUMBER = 5 THEN
	    UPDOWN = 고개각도_탈출상수
    ELSE
        UPDOWN = 고개각도_라인트레이싱상수
    ENDIF
    
    GOSUB 고개동작

    'ARROW = 100
    'GOSUB 고개좌우
    DELAY 2000
    

    GOTO 라인트레이싱


'***************************************************

라인트레이싱:
    ETX 4800,Line_detect

    GOTO 라인트레이싱_시작

'***************************************************

라인트레이싱_시작:
    ERX 4800, S, 라인트레이싱_시작

    ' 첫번째 구간(출발지점 ~ 방위 인식) -> 중앙정렬 + 앵글조정 + T 인식
    IF LINE_TRACING_NUMBER = 1 THEN
        IF S = Line_no THEN
            LINE_TRACING_NUMBER = LINE_TRACING_NUMBER + 1

            GOSUB 신호버리기

            UPDOWN = 100
            GOSUB 고개동작

            ' E -> 오른팔 앞으로 들기 + "동쪽 동쪽" 소리내기
            IF LETTER = Alpha_E THEN
                GOSUB 오른팔앞으로들기
                GOSUB 동쪽동쪽소리내기
                GOSUB 팔제자리
            ' W -> 왼팔 앞으로 들기 + "서쪽 서쪽" 소리내기
            ELSEIF LETTER = Alpha_W THEN
                GOSUB 왼팔앞으로들기
                GOSUB 서쪽서쪽소리내기
                GOSUB 팔제자리
            ' S -> 양손 뒤로 나란히 들기 + "남쪽 남쪽" 소리내기
            ELSEIF LETTER = Alpha_S THEN
                GOSUB 양손뒤로나란히들기
                GOSUB 남쪽남쪽소리내기
                GOSUB 팔제자리
            ' N -> 양손 앞으로 나란히 들기 + "북쪽 북쪽" 소리내기
            ELSEIF LETTER = Alpha_N THEN
                GOSUB 양손앞으로나란히들기
                GOSUB 북쪽북쪽소리내기
                GOSUB 팔제자리
            ENDIF

            GOSUB 후진

            IF DIRECTION = Arrow_left THEN
                GOSUB 좌회전60도     
            ELSEIF DIRECTION = Arrow_right THEN
                GOSUB 우회전60도
            ENDIF

            UPDOWN = 고개각도_라인트레이싱상수
            GOSUB 고개동작
            DELAY 1000
            GOTO 라인찾기
            
        ELSE
            ' 중앙정렬
            IF S = Line_mid THEN 
                GOSUB 정확전진
            ELSEIF S = Line_left THEN
                'FOR I = 1 TO 2
	            'GOSUB 왼쪽옆으로20 
	            'NEXT I
	            '보행횟수 = 1
	            'GOSUB 전진
                GOSUB 왼쪽옆으로20
                GOSUB 정확전진
            ELSEIF S = Line_right THEN
                'FOR I = 1 TO 2
	            'GOSUB 오른쪽옆으로20 
	            'NEXT I
	            '보행횟수 = 1
	            'GOSUB 전진
                GOSUB 오른쪽옆으로20
                GOSUB 정확전진
            ENDIF
            
            보행횟수=1
            GOSUB 전진

            GOSUB 앵글조정
            'GOTO T인식 ' T인식 제외하고 싶으면 이 부분 주석처리

        ENDIF

    ' 두번째 구간(방향 인식 ~ 첫번째 미션방) -> 중앙정렬 + 앵글조정
    ELSEIF LINE_TRACING_NUMBER = 2 THEN
        IF S = Line_no THEN
            LINE_TRACING_NUMBER = LINE_TRACING_NUMBER + 1 

            GOSUB 신호버리기

            UPDOWN = 고개각도_방이름인식상수
            GOSUB 고개동작
            DELAY 1000

            'GOSUB 후진

            GOTO 방이름인식
            
        ELSE
            ' 중앙정렬
            IF S = Line_mid THEN 
                GOSUB 정확전진
            ELSEIF S = Line_left THEN
                '' FOR I = 1 TO 2
	            'GOSUB 왼쪽옆으로20 
	            '' NEXT I
	            '보행횟수 = 1
	            'GOSUB 전진
                GOSUB 왼쪽옆으로20
                GOSUB 정확전진
            ELSEIF S = Line_right THEN
                '' FOR I = 1 TO 2
	            'GOSUB 오른쪽옆으로20 
	            '' NEXT I
	            '보행횟수 = 1
	            'GOSUB 전진
                GOSUB 오른쪽옆으로20
                GOSUB 정확전진
            ENDIF
            
            보행횟수=1
            GOSUB 전진

            GOSUB 앵글조정
        ENDIF

    ' 세번째 구간(첫번째 미션방 탈출 ~ 두번째 미션방) -> 중앙정렬 + 앵글조정
    ELSEIF LINE_TRACING_NUMBER = 3 THEN
        IF S = Line_no THEN
            LINE_TRACING_NUMBER = LINE_TRACING_NUMBER + 1 

            GOSUB 신호버리기

            UPDOWN = 고개각도_방이름인식상수
            GOSUB 고개동작
            DELAY 1000

            'GOSUB 후진
            GOTO 방이름인식

        ELSE
            ' 중앙정렬
            IF S = Line_mid THEN
                IF DIRECTION = Arrow_left THEN
                    GOSUB 정확전진
                    GOSUB 오른쪽옆으로20  
                ELSEIF DIRECTION = Arrow_right THEN  
                    GOSUB 정확전진 
                    GOSUB 왼쪽옆으로20
                ENDIF

                'GOSUB 정확전진

	        ELSEIF S = Line_left THEN
	            'FOR I = 1 TO 2
	            '	GOSUB 왼쪽옆으로20 
	            'NEXT I
	            '보행횟수 = 1
	            'GOSUB 전진
                GOSUB 왼쪽옆으로20
                GOSUB 정확전진
	        ELSEIF S = Line_right THEN
                'FOR I = 1 TO 2
	            '	GOSUB 오른쪽옆으로20 
	            'NEXT I
	            '보행횟수 = 1
	            'GOSUB 전진
                GOSUB 오른쪽옆으로20
                GOSUB 정확전진
	        ENDIF
	        
	        보행횟수=1
            GOSUB 전진
	
	        GOSUB 앵글조정
        ENDIF

    ' 네번째 구간(두번째 미션방 탈출 ~ 세갈래길 인식) -> 중앙정렬 + 앵글조정 + 세갈래길인식
    ' 라인이 인식되지 않는 경우는 없다고 가정
    ELSEIF LINE_TRACING_NUMBER = 4 THEN
    	IF S = Line_no THEN
            GOSUB 신호버리기

            ' 탈출 시 넘어졌을 경우 대비
            GOSUB 앞뒤기울기측정
            GOSUB 좌우기울기측정
            GOSUB 적외선거리센서확인
            
            IF 넘어진확인 = 1 THEN 
                넘어진확인 = 0
            ENDIF

            GOSUB 만세
            GOSUB 위험미션방소리내기
            GOSUB 기본자세

            ETX 4800, 16 ' 라즈베리파이코드 종료
            END
    	ENDIF
        
        IF S = Line_mid THEN
            GOSUB 정확전진
            GOSUB 앵글조정
            GOTO 세갈래길인식 ' 세갈래길 인식 시, LINE_TRACING_NUMBER 증가
        ELSE
            IF S = Line_left THEN
                GOSUB 왼쪽옆으로20 
            ELSEIF S = Line_right THEN
                GOSUB 오른쪽옆으로20 
            ENDIF

            GOSUB 앵글조정

        ENDIF

    ' 다섯번째 구간(세갈래길 인식 ~ 도착지점) -> 중앙정렬 + 앵글조정
    ' 천천히 걸어갈 필요 없다고 판단
    ELSEIF LINE_TRACING_NUMBER = 5 THEN
        IF S = Line_no THEN
            GOSUB 신호버리기

            ' 탈출 시 넘어졌을 경우 대비
            GOSUB 앞뒤기울기측정
            GOSUB 좌우기울기측정
            GOSUB 적외선거리센서확인
            
            IF 넘어진확인 = 1 THEN 
                넘어진확인 = 0
            ENDIF

            GOSUB 만세
            GOSUB 위험미션방소리내기
            GOSUB 기본자세

            ETX 4800, 16 ' 라즈베리파이코드 종료
            END
        ELSE
            IF S = Line_mid THEN
                GOSUB 정확전진
            ELSEIF S = Line_left THEN
                GOSUB 왼쪽옆으로20
                보행횟수 = 2
				GOSUB 전진 
            ELSEIF S = Line_right THEN
                GOSUB 오른쪽옆으로20 
                보행횟수 = 2
				GOSUB 전진
            ENDIF

            GOSUB 앵글조정
            
        ENDIF
    ENDIF

    GOTO MAIN

'***************************************************

라인찾기:
    ETX 4800,Yellow_detect

    GOTO 라인찾기_시작

'***************************************************

라인찾기_시작:
    ERX 4800, S, 라인찾기_시작

    ' 노란색이 인식되지 않았을 때 -> 좌/우회전
    IF S = Yellow_no THEN
        IF DIRECTION = Arrow_left THEN
            GOSUB 좌회전10도     
        ELSEIF DIRECTION = Arrow_right THEN
            GOSUB 우회전10도
        ENDIF

        GOTO 라인찾기
    ' 노란색이 인식되었을 때 -> 라인트레이싱 시작
    ELSE
        GOTO MAIN
    ENDIF

'***************************************************

앵글조정:
    ERX 4800, S, 앵글조정

    ' 선이 왼쪽으로 기울었을 때
    IF S = Line_angle_left THEN
        GOSUB 좌회전10도
        WAIT
         GOSUB 좌회전10도
    ' 선이 오른쪽으로 기울었을 때
    ELSEIF S = Line_angle_right THEN
        GOSUB 우회전10도
        WAIT
        GOSUB 우회전10도
    ENDIF

    RETURN

'***************************************************

T인식:
    ETX 4800,T_detect

    GOTO T인식_시작

T인식_시작:
    ERX 4800, S, T인식_시작

    ' T가 인식되었을 때
    IF S = T_yes THEN
        LINE_TRACING_NUMBER = LINE_TRACING_NUMBER + 1

        GOSUB 정확전진
        GOSUB 정확전진
        
        UPDOWN = 100
        GOSUB 고개동작

        ' E -> 오른팔 앞으로 들기 + "동쪽 동쪽" 소리내기
        IF LETTER = Alpha_E THEN
            GOSUB 오른팔앞으로들기
            GOSUB 동쪽동쪽소리내기
            GOSUB 팔제자리
        ' W -> 왼팔 앞으로 들기 + "서쪽 서쪽" 소리내기
        ELSEIF LETTER = Alpha_W THEN
            GOSUB 왼팔앞으로들기
            GOSUB 서쪽서쪽소리내기
            GOSUB 팔제자리
        ' S -> 양손 뒤로 나란히 들기 + "남쪽 남쪽" 소리내기
        ELSEIF LETTER = Alpha_S THEN
            GOSUB 양손뒤로나란히들기
            GOSUB 남쪽남쪽소리내기
            GOSUB 팔제자리
        ' N -> 양손 앞으로 나란히 들기 + "북쪽 북쪽" 소리내기
        ELSEIF LETTER = Alpha_N THEN
            GOSUB 양손앞으로나란히들기
            GOSUB 북쪽북쪽소리내기
            GOSUB 팔제자리
        ENDIF
        

        IF DIRECTION = Arrow_left THEN
            GOSUB 좌회전90도     
        ELSEIF DIRECTION = Arrow_right THEN
            GOSUB 우회전90도
        ENDIF
    ENDIF

    GOTO MAIN   

'***************************************************

세갈래길인식:
	ETX 4800,Three_way_detect
	
	GOTO 세갈래길인식_시작

세갈래길인식_시작:
    ERX 4800, S, 세갈래길인식_시작

    '' 이전까지 세갈래길이 인식되다가 더 이상 인식되지 않을 때
    'IF IS_T = TRUE AND S = Line_threeway_no THEN
    '    LINE_TRACING_NUMBER = LINE_TRACING_NUMBER + 1 
'
    '    ' GOSUB 후진 ' 세갈래길을 너무 벗어나는 경우 -> 주석 해제
'
    '    IF DIRECTION = Arrow_left THEN
    '        GOSUB 좌회전60도
    '    ELSEIF DIRECTION = Arrow_right THEN
    '        GOSUB 우회전60도
    '    ENDIF  
'
    'ENDIF
'
    'IF S = Line_threeway_yes THEN
    '    IS_T = TRUE
    '    GOSUB 정확전진
'
    '    GOTO 세갈래길인식
    'ENDIF
'
	'	GOTO MAIN

    IF S = Line_threeway_yes THEN
        GOSUB 정확전진
        GOSUB 정확전진
        GOSUB 정확전진
        
        IF DIRECTION = Arrow_left THEN
            GOSUB 왼쪽옆으로70연속
        ELSEIF DIRECTION = Arrow_right THEN
            GOSUB 오른쪽옆으로70연속
        ENDIF

		LINE_TRACING_NUMBER = LINE_TRACING_NUMBER + 1

        IF DIRECTION = Arrow_left THEN
            GOSUB 좌회전60도
        ELSEIF DIRECTION = Arrow_right THEN
            GOSUB 우회전60도
        ENDIF

        UPDOWN = 고개각도_라인트레이싱상수
        GOSUB 고개동작
        GOTO 라인찾기
    
    ENDIF
    
    GOTO MAIN


'***************************************************

방이름인식:
    ETX  4800,Alpha_detect

    GOTO 방이름인식_시작

'***************************************************

방이름인식_시작:
    ERX 4800,S,방이름인식_시작

    ' 인식하지 못한 경우 -> 한 바퀴 턴을 돌며 파악
    IF S = Alpha_no THEN
        IF DIRECTION = Arrow_left THEN
            GOSUB 좌회전10도
        ELSEIF DIRECTION = Arrow_right THEN
            GOSUB 우회전10도
        ENDIF

        GOTO 방이름인식

    ' 방이름을 인식한 경우
    ELSE
        ROOM = S ' 방이름 저장
        GOTO 미션

    ENDIF
    
'***************************************************

방위인식:
    ETX  4800,Alpha_NWES

    GOTO 방위인식_시작

'***************************************************

방위인식_시작:
    ERX 4800,S,방위인식_시작

    ' 인식하지 못한 경우 -> 전진
    IF S = Alpha_no THEN
        'GOSUB 정확전진
        GOTO 방위인식

    ' 방위를 인식한 경우
    ELSE
        LETTER = S ' 방위 저장
        UPDOWN = 고개각도_방향인식상수
        GOSUB 고개동작
        DELAY 2000
        GOTO 방향인식

    ENDIF

'***************************************************

방향인식:
    ETX  4800,Arrow_detect

    GOTO 방향인식_시작

'***************************************************

방향인식_시작:
    ERX 4800,S,방향인식_시작

    ' 방향 인식안됨 
    IF S = Arrow_no THEN
        GOSUB 정확전진
        GOTO 방향인식

    ' 방향 인식되었을 때 -> 라인트레이싱 시작
    ELSE
        DIRECTION = S

        GOTO MAIN
    ENDIF

'***************************************************

미션:
    ' 화살표 방향이 왼쪽이였을때 -> 왼쪽횡무빙 + 직진 + 미션 + 우회전 + 직진 + 오른쪽횡무빙 + 직진 
    IF DIRECTION = Arrow_left THEN
        FOR I = 1 TO 미션방_횡이동입장상수
        GOSUB 왼쪽옆으로70연속
        DELAY 500
        NEXT I
    ' 화살표 방향이 오른쪽이였을때 -> 오른쪽횡무빙 + 직진 + 미션 + 좌회전 + 직진 + 왼쪽횡무빙 + 직진 
    ELSEIF DIRECTION = Arrow_right THEN
        FOR I = 1 TO 미션방_횡이동입장상수
        GOSUB 오른쪽옆으로70연속
        DELAY 500
        NEXT I
    ENDIF

    UPDOWN = 미션방_종확인
    GOSUB 고개동작
    DELAY 1000

    GOTO 미션확인   

'***************************************************

미션확인:
    ETX 4800, Yellow_detect

    GOTO 미션확인_시작

'***************************************************

미션확인_시작:
    ERX 4800, S, 미션확인_시작
    
    '  노란색 종 인식됨 -> 계단지역 미션
    IF S = Yellow_yes THEN
        UPDOWN = 계단_인식상수
        GOSUB 고개동작
        DELAY 1000

        GOTO 계단찾기
    ' 초록색 인식되지 않음 -> 위험지역::시민구출 미션
    ELSE
        DANGEROUS_ROOM = ROOM ' 위험지역 방 이름 저장
        GOSUB 위험지역소리내기
        
        UPDOWN = 위험_시민인식상수
        GOSUB 고개동작
        DELAY 1000

        GOTO 시민찾기
    ENDIF

'***************************************************

계단찾기:
    ETX 4800, Green_detect

    GOTO 계단찾기_시작

'***************************************************

계단찾기_시작:
    ERX 4800, S, 계단찾기_시작

    ' 계단이 인식되었을 때 -> 계단 오르기 (전진은 계단오르기 label에서 실행)
    IF S = Green_yes THEN
        UPDOWN = 계단_오르기상수
        보행횟수 =1
        GOSUB 전진
        WAIT
        GOSUB 고개동작
        DELAY 1000

        GOTO 계단오르기 
    ' 계단이 인식되지 않았을 때 -> 전진 후 다시 계단 찾기
    ELSE
		IF GREEN_WALK_CNT = 계단_초록색전진인식상수 THEN
        	UPDOWN = 미션방_종확인
        	GOSUB 고개동작
        	DELAY 1000		
		
			GOTO 종찾기
		ELSE
			보행횟수 = 1
        	GOSUB 전진
        	
        	GREEN_WALK_CNT = GREEN_WALK_CNT + 1
        	
        	GOTO 계단찾기
		ENDIF

    ENDIF

'***************************************************

종찾기:
    ETX 4800, Bell_detect

    GOTO 종찾기_시작	

'***************************************************

종찾기_시작:
	ERX 4800, S, 종찾기_시작
	
	IF S = Line_no THEN
		GOSUB 좌회전30도
		
		GOTO 종찾기
	ELSE	
		IF S = Line_mid THEN 
            보행횟수 = 2
	        GOSUB 전진

		    UPDOWN = 계단_인식상수
        	GOSUB 고개동작
        	DELAY 1000

        	GOTO 계단찾기	
        ELSEIF S = Line_left THEN
            FOR I = 1 TO 2
            GOSUB 왼쪽옆으로20 
	        NEXT I   
        ELSEIF S = Line_right THEN
            FOR I = 1 TO 2
	        GOSUB 오른쪽옆으로20 
	        NEXT I
        ENDIF
	
		GOTO 종찾기
	ENDIF

'***************************************************

계단오르기:
    ' 첫번째 계단을 오를 때 (초록색에 위치해있을 때)
    IF STAIRS_CNT = 0 THEN
		보행횟수 = 2
	    GOSUB 전진
		DELAY 3000
		WAIT
		보행횟수 = 1
	    GOSUB 전진
		DELAY 3000
		WAIT
		GOSUB 계단걸음
        DELAY 2500
        WAIT
        GOSUB 계단걸음
        DELAY 2500
        WAIT
        GOSUB 계단걸음
        DELAY 2500
        WAIT
        WAIT
        GOSUB All_motor_mode3
        SPEED 4
        MOVE G6A,100,  76, 145,  93, 100, 100
        MOVE G6D,100,  76, 145,  93, 100, 100
        MOVE G6B,100,  40,  90,
        MOVE G6C,100,  40,  90,
        WAIT
        DELAY 500
        SPEED 6
        GOSUB 기본자세
        
        DELAY 1500
        WAIT
        
        GOSUB 좌우기울기측정
        DELAY 2500
        WAIT
        
        GOSUB 계단오른발오르기2CM
        WAIT
		DELAY 500
    ' 두번째 계단을 오를 때 (빨간색에 위치해있을 때)
    ELSEIF STAIRS_CNT = 1 THEN
        GOSUB 계단걸음
        DELAY 2500
        WAIT
        GOSUB 계단걸음
        DELAY 2500
        WAIT
        GOSUB 계단걸음
        DELAY 2500
        WAIT
        GOSUB All_motor_mode3
        SPEED 4
        MOVE G6A,100,  76, 145,  93, 100, 100
        MOVE G6D,100,  76, 145,  93, 100, 100
        MOVE G6B,100,  40,  90,
        MOVE G6C,100,  40,  90,
        WAIT
        DELAY 500
        SPEED 6
        GOSUB 기본자세
        
        DELAY 1500
        WAIT
        
        GOSUB 좌우기울기측정
        DELAY 2500
        WAIT
        
        GOSUB 계단오른발오르기2CM
        WAIT
	ENDIF

    

    ' 넘어짐 여부 확인
    GOSUB 앞뒤기울기측정
    DELAY 3000

    ' 넘어진게 맞았다면 -> 현재 오른 계단 층수(STAIRS_CNT)를 0으로 초기화 + 다시 계단 찾기
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0
        STAIRS_CNT = 0
 
        UPDOWN = 계단_인식상수
        GOSUB 고개동작
        DELAY 1000

        GOTO 계단찾기
    ' 무사히 계단을 올랐다면 -> 현재 오른 계단 층수에 따라 행동 결정
    ELSE
        STAIRS_CNT = STAIRS_CNT + 1 

        ' 만약 계단을 끝까지 다 올랐다면 -> 종치기
        IF STAIRS_CNT = 2 THEN 
            GOTO 종치기
        ' 아직 계단이 남았다면 -> 계단 오르기
        ELSE
            GOTO 계단오르기
        ENDIF
    ENDIF

'***************************************************

종치기:
    ' 화살표 방향이 왼쪽이였을때 -> 오른쪽 턴하면서 종치기
    IF DIRECTION = Arrow_left THEN
        보행횟수 = 1
        GOSUB 전진
        DELAY 500

        GOSUB 구조요청소리내기

        GOSUB 우회전종치기
        DELAY 500

        보행횟수=1
        GOSUB 전진
        DELAY 500  
    ' 화살표 방향이 오른쪽이였을때 -> 왼쪽 턴하면서 종치기
    ELSEIF DIRECTION = Arrow_right THEN
        보행횟수 = 1
        GOSUB 전진
        DELAY 500
        DELAY 300
   		GOSUB 정확전진
   		WAIT

        GOSUB 구조요청소리내기

        GOSUB 좌회전종치기
        DELAY 500

        보행횟수=1
        GOSUB 전진
        DELAY 500  
        보행횟수=1
        GOSUB 전진
        DELAY 500 
    ENDIF

    GOSUB 기어가기
    DELAY 500
    WAIT
    GOSUB 기어가기
    DELAY 500
    WAIT
    DELAY 300
    GOSUB 정확전진
    WAIT
    DELAY 300
    GOSUB 정확전진
    WAIT
   
    UPDOWN = 고개각도_라인트레이싱상수
    GOSUB 고개동작
    DELAY 1000

    GOTO 라인트레이싱복귀    

'***************************************************

시민찾기: 
    ETX 4800, BluePerson_detect

    ' 시민을 못찾은 경우 -> 고개 각도 조절 및 횡무빙을 통해 시민찾기
    IF IS_PERSON = FALSE THEN
        GOTO 시민찾기_시작
    ' 시민을 찾은 경우 -> 시민에게 다가가기
    ELSE
        GOTO 시민에게다가가기
    ENDIF

'***************************************************

시민찾기_시작:
    ERX 4800, S, 시민찾기_시작

    ' 시민이 보이지 않을 때 -> 고개 각도 조절 및 횡무빙
    IF S = BluePerson_detect_no THEN
        ' 고개를 최대한 내렸는데도 시민이 보이지 않을 때 -> 횡무빙
        IF UPDOWN < 25 THEN
            ' 화살표 방향이 왼쪽이였을때 -> 오른쪽횡무빙
            IF DIRECTION = Arrow_left THEN
                FOR I = 1 TO 위험_횡이동횟수상수
                    GOSUB 오른쪽옆으로70연속
                NEXT I
            ' 화살표 방향이 오른쪽이였을때 -> 왼쪽쪽횡무빙
            ELSEIF DIRECTION = Arrow_right THEN
                FOR I = 1 TO 위험_횡이동횟수상수
                    GOSUB 왼쪽옆으로70연속
                NEXT I
            ENDIF

            UPDOWN = 위험_시민인식상수
            GOSUB 고개동작
            DELAY 3000
        ' 고개 내려서 시민 다시 찾기
        ELSE
            UPDOWN = UPDOWN - 25
            GOSUB 고개동작
            DELAY 4000
        ENDIF
    ' 시민 찾았을 때 -> 위험지역 소리내기 + 시민에게 다가가기
    ELSE
        IS_PERSON = TRUE
    ENDIF

    GOTO 시민찾기

'***************************************************

시민에게다가가기:
    ERX 4800, S, 시민에게다가가기

    ' 시민이 보이지 않을 때 -> 고개 각도 조절 통해 시민 찾기 또는 시민 잡기
    IF S = BluePerson_detect_no THEN
        IF UPDOWN < 56 THEN
        
        	
        	GOSUB 정확전진
        	WAIT
        	DELAY 1000
        	
            GOSUB 시민잡기
            WAIT
            DELAY 1000
            
            IF DIRECTION = Arrow_left THEN
                FOR I = 1 TO 3
                	GOSUB 집고오른쪽턴45
                	DELAY 3000
                NEXT I
            
            ELSEIF DIRECTION = Arrow_right THEN
                FOR I = 1 TO 3
                	GOSUB 집고왼쪽턴45
                	DELAY 3000
                NEXT I
            ENDIF
            
            WAIT
            DELAY 2000
            


			
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			GOSUB 집고전진
			DELAY 1000
			WAIT
			
			
			
			GOSUB 시민놓기
			
			GOTO 라인트레이싱복귀
            
        ELSE
        	
            UPDOWN = UPDOWN - 25
            GOSUB 고개동작
            DELAY 4000
        ENDIF
    
    ' 시민이 보일 때 -> 시민 트레이싱
    ELSE
        IF S = BluePerson_detect_mid THEN
            GOSUB 정확전진

        ELSEIF S = BluePerson_detect_left THEN
             GOSUB 왼쪽횡으로이동

        ELSEIF S = BluePerson_detect_right THEN
             GOSUB 오른쪽횡으로이동

        ENDIF

    ENDIF

    GOTO 시민찾기

'***************************************************

라인트레이싱복귀:
    ETX 4800, Yellow_detect

    GOTO 라인트레이싱복귀_시작

'***************************************************

라인트레이싱복귀_시작:
    ERX 4800, S, 라인트레이싱복귀_시작
    WAIT
    UPDOWN = 고개각도_라인트레이싱상수
    GOSUB 고개동작
    DELAY 1000
	WAIT
    ' 노란색이 인식되지 않으면 -> 횡무빙
    IF S = Yellow_no THEN
        ' 화살표 방향이 왼쪽이였을때 -> 오른쪽횡무빙
        IF DIRECTION = Arrow_left THEN
            GOSUB 오른쪽옆으로70연속
        ' 화살표 방향이 오른쪽이였을때 -> 왼쪽횡무빙
        ELSEIF DIRECTION = Arrow_right THEN
            GOSUB 왼쪽옆으로70연속
        ENDIF

        GOTO 라인트레이싱복귀
    ' 노란색이 인식되면 -> 라인트레이싱 시작
    ELSE
        IF DIRECTION = Arrow_left THEN
            FOR i = 1 TO 1
                GOSUB 오른쪽옆으로70연속
            NEXT i
        ' 화살표 방향이 오른쪽이였을때 -> 왼쪽횡무빙
        ELSEIF DIRECTION = Arrow_right THEN
            FOR i = 1 TO 1
                GOSUB 왼쪽옆으로70연속
            NEXT i
        ENDIF

        GOTO MAIN

    ENDIF

'***************************************************



'*************************************************************************
'*************************************************************************
'*************************** 사용자 정의 label ******************************
'*************************************************************************
    '******************************************

신호버리기:
    ERX 4800,S,신호버리기

    RETURN

위험미션방소리내기:
    ' 위험미션방이 A일 때
    IF DANGEROUS_ROOM = Alpha_A THEN
	    GOSUB A소리내기

    ' 위험미션방이 B일 때
    ELSEIF DANGEROUS_ROOM = Alpha_B THEN
	    GOSUB B소리내기

    ' 위험미션방이 C일 때
    ELSEIF DANGEROUS_ROOM = Alpha_C THEN
	    GOSUB C소리내기

    ' 위험미션방이 D일 때
    ELSEIF DANGEROUS_ROOM = Alpha_D THEN
	    GOSUB D소리내기

    ENDIF

    RETURN 

오른쪽옆으로70연속:
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2

오른쪽옆으로70연속_loop:
    DELAY  10

    SPEED 10
    MOVE G6D, 90,  90, 120, 105, 110, 100
    MOVE G6A,100,  76, 145,  93, 107, 100
    'MOVE G6C,100,  40
    'MOVE G6B,100,  40
    WAIT

    SPEED 13
    MOVE G6D, 102,  76, 145, 93, 100, 100
    MOVE G6A,83,  78, 140,  96, 115, 100
    WAIT

    SPEED 13
    MOVE G6D,98,  76, 145,  93, 100, 100
    MOVE G6A,98,  76, 145,  93, 100, 100
    WAIT

    SPEED 12
    MOVE G6A,100,  76, 145,  93, 100, 100
    MOVE G6D,100,  76, 145,  93, 100, 100
    WAIT


    '  ERX 4800, A ,오른쪽옆으로70연속_loop
    '    IF A = A_OLD THEN  GOTO 오른쪽옆으로70연속_loop
    '오른쪽옆으로70연속_stop:
    GOSUB 기본자세

    RETURN
    '**********************************************

왼쪽옆으로70연속:
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2
왼쪽옆으로70연속_loop:
    DELAY  10

    SPEED 10
    MOVE G6A, 90,  90, 120, 105, 110, 100	
    MOVE G6D,100,  76, 145,  93, 107, 100	
    'MOVE G6C,100,  40
    'MOVE G6B,100,  40
    WAIT

    SPEED 13
    MOVE G6A, 102,  76, 145, 93, 100, 100
    MOVE G6D,83,  78, 140,  96, 115, 100
    WAIT

    SPEED 13
    MOVE G6A,98,  76, 145,  93, 100, 100
    MOVE G6D,98,  76, 145,  93, 100, 100
    WAIT

    SPEED 12
    MOVE G6D,100,  76, 145,  93, 100, 100
    MOVE G6A,100,  76, 145,  93, 100, 100
    WAIT

    '   ERX 4800, A ,왼쪽옆으로70연속_loop	
    '    IF A = A_OLD THEN  GOTO 왼쪽옆으로70연속_loop
    '왼쪽옆으로70연속_stop:

    GOSUB 기본자세

    RETURN

    '**********************************************
    한걸음걷기:
    보행속도 = 12
    좌우속도 = 4
    넘어진확인 = 0
    MOVE G6A, 100,  76, 145,  93, 100, 100
    MOVE G6D, 100,  76, 145,  93, 100, 100
    MOVE G6B, 100,  30,  80, 100, 100, 102
    MOVE G6C, 100,  30,  80, 100,  89, 100
    WAIT


    GOSUB Leg_motor_mode3
    HIGHSPEED SETON
    SPEED 10
    MOVE G6D,  90,  74, 144,  95, 110
    MOVE G6A, 108,  76, 146,  93, 96
    MOVE G6C, 100
    MOVE G6B, 100
    WAIT

    SPEED 12
    MOVE G6D,90, 90, 120, 105, 110,100
    MOVE G6A,108,  76, 147,  93,  96,100
    MOVE G6C,90
    MOVE G6B,110
    WAIT

    HIGHSPEED SETOFF
    GOTO 한걸음걷기_2	

한걸음걷기_2:
    MOVE G6A,110,  76, 147,  93, 100,100
    MOVE G6D,96, 90, 120, 102, 107,100
    MOVE G6B,110
    MOVE G6C,90
    WAIT

한걸음걷기_3:
    ETX 4800,13 '진행코드를 보냄

    SPEED 보행속도

    MOVE G6D, 90,  56, 145, 115, 112
    MOVE G6A,108,  76, 147,  93,  96
    WAIT

    SPEED 좌우속도
    MOVE G6D,108,  76, 147, 93,  98
    MOVE G6A,90, 100, 145,  69, 108
    WAIT

    SPEED 보행속도

    GOSUB 앞뒤기울기측정
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0
        GOTO MAIN
    ENDIF


    ERX 4800,A, 한걸음걷기_4
    IF A = 11 THEN
        GOTO 한걸음걷기_4
        '    ELSE
        '    	MOVE G6A, 90, 100, 100, 115, 110,100
        ' 		MOVE G6D,112,  76, 146,  93,  96,100
        ' 		MOVE G6B,90
        ' 		MOVE G6C,110
        ' 		WAIT
        ' 		HIGHSPEED SETOFF
        ' 		SPEED 8

        ' 		MOVE G6D, 106,  76, 146,  93,  96,100		
        ' 		MOVE G6A,  88,  71, 152,  91, 106,100
        ' 		MOVE G6C, 100
        ' 		MOVE G6B, 100
        ' 		WAIT	
        ' 		SPEED 8
        ' 		GOSUB 기본자세2

        ' 		GOTO RX_EXIT
    ENDIF
한걸음걷기_4:
    SPEED 13
    MOVE G6A,95, 90, 120, 105, 111,100
    MOVE G6D,108,  76, 146,  93,  96,100
    MOVE G6B, 90
    MOVE G6C,110
    WAIT

    ' SPEED 10
    '  GOSUB 기본자세2
    ' GOTO 한걸음걷기

    SPEED 10
    GOSUB 기본자세2
    RETURN

    '*******************************************************************************************************************************
한걸음걷기2:
    보행속도 = 8
    좌우속도 = 4
    넘어진확인 = 0
    MOVE G6D, 100,  76, 145,  93, 100, 100
    MOVE G6A, 100,  76, 145,  93, 100, 100
    MOVE G6B, 100,  30,  80, 100, 100, 102
    MOVE G6C, 100,  30,  80, 100,  89, 100
    WAIT


    GOSUB Leg_motor_mode3
    HIGHSPEED SETON
    SPEED 10
    MOVE G6A,  90,  74, 144,  95, 110
    MOVE G6D, 108,  76, 146,  93, 96
    MOVE G6C, 100
    MOVE G6B, 100
    WAIT

    SPEED 12
    MOVE G6A,90, 90, 120, 105, 110,100
    MOVE G6D,108,  76, 147,  93,  96,100
    MOVE G6C,90
    MOVE G6B,110
    WAIT

    HIGHSPEED SETOFF
    GOTO 한걸음걷기2_2	

한걸음걷기2_2:
    MOVE G6D,110,  76, 147,  93, 100,100
    MOVE G6A,96, 90, 120, 102, 107,100
    MOVE G6B,110
    MOVE G6C,90
    WAIT

한걸음걷기2_3:
    ETX 4800,13 '진행코드를 보냄

    SPEED 보행속도

    MOVE G6A, 90,  56, 145, 115, 112
    MOVE G6D,108,  76, 147,  93,  96
    WAIT

    SPEED 좌우속도
    MOVE G6A,108,  76, 147, 93,  98
    MOVE G6D,90, 100, 145,  69, 108
    WAIT

    SPEED 보행속도

    GOSUB 앞뒤기울기측정
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0
        GOTO MAIN
    ENDIF


    ERX 4800,A, 한걸음걷기2_4
    IF A = 11 THEN
        GOTO 한걸음걷기2_4
        '    ELSE
        '    	MOVE G6A, 90, 100, 100, 115, 110,100
        ' 		MOVE G6D,112,  76, 146,  93,  96,100
        ' 		MOVE G6B,90
        ' 		MOVE G6C,110
        ' 		WAIT
        ' 		HIGHSPEED SETOFF
        ' 		SPEED 8

        ' 		MOVE G6D, 106,  76, 146,  93,  96,100		
        ' 		MOVE G6A,  88,  71, 152,  91, 106,100
        ' 		MOVE G6C, 100
        ' 		MOVE G6B, 100
        ' 		WAIT	
        ' 		SPEED 8
        ' 		GOSUB 기본자세2

        ' 		GOTO RX_EXIT
    ENDIF
한걸음걷기2_4:
    SPEED 9
    MOVE G6D,95, 90, 120, 105, 111,100
    MOVE G6A,108,  76, 146,  93,  96,100
    MOVE G6B, 90
    MOVE G6C,110
    WAIT

    SPEED 6
    'GOSUB 기본자세2
    RETURN
    '*******************************************************
    '********************************************
    
집고전진:
    보행속도 = 8
    좌우속도 = 4
    넘어진확인 = 0


    'GOSUB 전방하향18도
    'DELAY 20
    SPEED 10
    GOSUB All_motor_mode3
    MOVE G6B,175,  10,  60
    MOVE G6C,175,  10,  60
    WAIT

    DELAY 20
    'HIGHSPEED SETON

    SPEED 10
    MOVE G6D,  90,  74, 142,  94, 109
    MOVE G6A, 108,  76, 144,  93, 96
    WAIT

    SPEED 12
    MOVE G6D,90, 90, 118, 101, 109,99
    MOVE G6A,108,  76, 145,  92,  96,100
    WAIT

    'HIGHSPEED SETOFF
    GOTO 집고전진_2	

집고전진_2:
    MOVE G6A,110,  76, 145,  93, 100,100
    MOVE G6D,96, 90, 118, 101, 106,99
    WAIT

집고전진_3:
    SPEED 보행속도

    MOVE G6D, 90,  56, 143, 114, 109
    MOVE G6A,108,  76, 145,  89,  95
    WAIT

    SPEED 좌우속도
    MOVE G6D,108,  76, 145, 89,  97
    MOVE G6A,90, 100, 140,  68, 107
    WAIT

    SPEED 보행속도

    GOSUB 앞뒤기울기측정
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0
        GOTO MAIN
    ENDIF


    ERX 4800,A, 집고전진_4
    IF A = 11 THEN
        GOTO 집고전진_4
        '    ELSE
        '    	MOVE G6A, 90, 100, 100, 115, 110,100
        ' 		MOVE G6D,112,  76, 146,  93,  96,100
        ' 		MOVE G6B,90
        ' 		MOVE G6C,110
        ' 		WAIT
        ' 		HIGHSPEED SETOFF
        ' 		SPEED 8

        ' 		MOVE G6D, 106,  76, 146,  93,  96,100		
        ' 		MOVE G6A,  88,  71, 152,  91, 106,100
        ' 		MOVE G6C, 100
        ' 		MOVE G6B, 100
        ' 		WAIT	
        ' 		SPEED 8
        ' 		GOSUB 기본자세2

        ' 		GOTO RX_EXIT
    ENDIF
집고전진_4:
    SPEED 13
    MOVE G6A,95, 90, 118, 101, 110,99
    MOVE G6D,108,  76, 144,  92,  95,99
    WAIT

    SPEED 11
    MOVE G6A,100,  76, 143,  92, 99, 100
    MOVE G6D,100,  76, 143,  92, 99, 100
    WAIT

    RETURN
    '*******************
    '************************************************
Leg_motor_mode1:
    MOTORMODE G6A,1,1,1,1,1
    MOTORMODE G6D,1,1,1,1,1
    RETURN
    '************************************************
        '**********************************************
집고오른쪽턴45:

    GOSUB Leg_motor_mode2
    SPEED 4
    MOVE G6A,95,  46, 145,  115, 105, 100
    MOVE G6D,95,  106, 145,  55, 105, 100
    WAIT

    SPEED 6
    MOVE G6A,93,  46, 145,  115, 105, 100
    MOVE G6D,93,  106, 145,  55, 105, 100
    WAIT

    SPEED 4
    MOVE G6A,100,  76, 145,  85, 100
    MOVE G6D,100,  76, 145,  85, 100
    WAIT
    GOSUB Leg_motor_mode1
    WAIT
    RETURN
    '**********************************************
    
'*************************************************************************
집고왼쪽턴45:

    GOSUB Leg_motor_mode2
    SPEED 4
    MOVE G6A,95,  106, 145,  55, 105, 100
    MOVE G6D,95,  46, 145,  115, 105, 100
    WAIT
    

    SPEED 6
    MOVE G6A,93,  106, 145,  55, 105, 100
    MOVE G6D,93,  46, 145,  115, 105, 100
    WAIT

    SPEED 4
    MOVE G6A,100,  76, 145,  85, 100
    MOVE G6D,100,  76, 145,  85, 100
    WAIT
    GOSUB Leg_motor_mode1
    WAIT
    RETURN
'*************************************************************************
기어가기:

    GOSUB Leg_motor_mode3
    SPEED 7
    MOVE G6A,100, 155,  28, 140, 100, 100
    MOVE G6D,100, 155,  28, 140, 100, 100
    MOVE G6B,180,  40,  85
    MOVE G6C,180,  40,  85
    WAIT

    SPEED 3	
    MOVE G6A, 100, 155,  53, 160, 100, 100
    MOVE G6D, 100, 155,  53, 160, 100, 100
    MOVE G6B,190,  30, 80
    MOVE G6C,190,  30, 80
    WAIT	

    GOSUB All_motor_mode2

    DELAY 300

    SPEED 8




    MOVE G6A, 100, 160,  55, 160, 100
    MOVE G6D, 100, 145,  75, 160, 100
    MOVE G6B, 175,  25,  70
    MOVE G6C, 190,  50,  40
    WAIT



    MOVE G6A, 100, 150,  70, 160, 100
    MOVE G6D, 100, 140, 120, 120, 100
    MOVE G6B, 160,  25,  70
    MOVE G6C, 190,  25,  70
    WAIT

    MOVE G6D, 100, 160,  55, 160, 100
    MOVE G6A, 100, 145,  75, 160, 100
    MOVE G6C, 175,  25,  70
    MOVE G6B, 190,  50,  40
    WAIT




    MOVE G6D, 100, 140,  80, 160, 100
    MOVE G6A, 100, 140, 120, 120, 100
    MOVE G6C, 160,  25,  70
    MOVE G6B, 190,  25,  70
    WAIT
    
    MOVE G6A, 100, 160,  55, 160, 100
    MOVE G6D, 100, 145,  75, 160, 100
    MOVE G6B, 175,  25,  70
    MOVE G6C, 190,  50,  40
    WAIT



    MOVE G6A, 100, 150,  70, 160, 100
    MOVE G6D, 100, 140, 120, 120, 100
    MOVE G6B, 160,  25,  70
    MOVE G6C, 190,  25,  70
    WAIT

    MOVE G6D, 100, 160,  55, 160, 100
    MOVE G6A, 100, 145,  75, 160, 100
    MOVE G6C, 175,  25,  70
    MOVE G6B, 190,  50,  40
    WAIT




    MOVE G6D, 100, 140,  80, 160, 100
    MOVE G6A, 100, 140, 120, 120, 100
    MOVE G6C, 160,  25,  70
    MOVE G6B, 190,  25,  70
    WAIT


    MOVE G6A, 100, 160,  55, 160, 100
    MOVE G6D, 100, 145,  75, 160, 100
    MOVE G6B, 175,  25,  70
    MOVE G6C, 190,  50,  40
    WAIT



    MOVE G6A, 100, 150,  70, 160, 100
    MOVE G6D, 100, 140, 120, 120, 100
    MOVE G6B, 160,  25,  70
    MOVE G6C, 190,  25,  70
    WAIT

    MOVE G6D, 100, 160,  55, 160, 100
    MOVE G6A, 100, 145,  75, 160, 100
    MOVE G6C, 175,  25,  70
    MOVE G6B, 190,  50,  40
    WAIT




    MOVE G6D, 100, 140,  80, 160, 100
    MOVE G6A, 100, 140, 120, 120, 100
    MOVE G6C, 160,  25,  70
    MOVE G6B, 190,  25,  70
    WAIT
    
    GOSUB 기어가다일어나기


 RETURN
 '**********************************************8
기어가다일어나기:
    PTP SETON		
    PTP ALLON
    SPEED 15
    HIGHSPEED SETOFF


    MOVE G6A, 100, 150,  80, 150, 100
    MOVE G6D, 100, 150,  80, 150, 100
    MOVE G6B,185,  40, 60
    MOVE G6C,185,  40, 60
    WAIT

    GOSUB Leg_motor_mode3
    DELAY 300

    SPEED 10	
    MOVE G6A,  100, 165,  25, 162, 100
    MOVE G6D,  100, 165,  25, 162, 100
    MOVE G6B,  155, 15, 90
    MOVE G6C,  155, 15, 90
    WAIT

    SPEED 10	
    MOVE G6A,  100, 150,  25, 162, 100
    MOVE G6D,  100, 150,  25, 162, 100
    MOVE G6B,  140, 15, 90
    MOVE G6C,  140, 15, 90
    WAIT

    SPEED 6
    MOVE G6A,  100, 138,  25, 155, 100
    MOVE G6D,  100, 138,  25, 155, 100
    MOVE G6B, 113,  30, 80
    MOVE G6C, 113,  30, 80
    WAIT

    DELAY 100
    SPEED 8
    GOSUB Leg_motor_mode2

    SPEED 6
    MOVE G6A,100, 140,  37, 140, 100, 100
    MOVE G6D,100, 140,  37, 140, 100, 100
    WAIT

    GOSUB 기본자세
    WAIT
    GOSUB 앞뒤기울기측정
    
    IF 넘어진확인= 1 THEN
       넘어진확인= 0
       
    ENDIF
    GOSUB 기본자세2

    RETURN
'*************************************************************************
좌우기울기측정:
    
        B = AD(좌우기울기AD포트)	'기울기 좌우
        IF B > 87 THEN
        WAIT
        
	    MOTORMODE G6A,3,3,3,3,2
	    MOTORMODE G6D,3,3,3,3,2
	    SPEED 5
	    MOVE G6A,97,  66, 145,  103, 103, 100
	    MOVE G6D,97,  86, 145,  83, 103, 100
	    WAIT
	
	    SPEED 12
	    MOVE G6A,94,  66, 145,  103, 101, 100
	    MOVE G6D,94,  86, 145,  83, 101, 100
	    WAIT
	    SPEED 6
	    MOVE G6A,101,  76, 146,  93, 98, 100
	    MOVE G6D,101,  76, 146,  93, 98, 100
	    WAIT
	
	    MOVE G6A,100,  76, 145,  93, 100, 100
	    MOVE G6D,100,  76, 145,  93, 100, 100
	    MOVE G6B,100,  30,  80,
	    MOVE G6C,100,  30,  80

		WAIT
		ENDIF
		
		IF 84 < B AND B < 87 THEN
		WAIT
		MOVE G6A, 100,  76, 145,  93, 100,  
		MOVE G6D, 100,  76, 145,  93, 100,  
		MOVE G6B, 100,  30,  80,  ,  ,  
		MOVE G6C, 100,  30,  80,  ,  ,  
		WAIT
		ENDIF
		


        IF  B < 84 THEN 
        WAIT
        
        
        MOTORMODE G6A,3,3,3,3,2
	    MOTORMODE G6D,3,3,3,3,2
	    SPEED 5
	    MOVE G6A,97,  86, 145,  83, 103, 100
	    MOVE G6D,97,  66, 145,  103, 103, 100
	    WAIT
	
	    SPEED 12
	    MOVE G6A,94,  86, 145,  83, 101, 100
	    MOVE G6D,94,  66, 145,  103, 101, 100
	    WAIT
	
	    SPEED 6
	    MOVE G6A,101,  76, 146,  93, 98, 100
	    MOVE G6D,101,  76, 146,  93, 98, 100
	    WAIT
	
	    MOVE G6A,100,  76, 145,  93, 100, 100
	    MOVE G6D,100,  76, 145,  93, 100, 100
	    MOVE G6B,100,  30,  80,
	    MOVE G6C,100,  30,  80

		WAIT
		ENDIF
		
		

        DELAY 기울기확인시간
        
        DELAY 300
        WAIT
       MOVE G6A,100,  76, 145,  93, 100, 100
   	   MOVE G6D,100,  76, 145,  93, 100, 100
  	   MOVE G6B,100,  30,  80,
  	   MOVE G6C,100,  30,  80
    WAIT


        
   
    RETURN
    '******************************************'
    
    '*********************************************'

정확전진:
	
    보행COUNT = 0
    보행속도 = 9
    좌우속도 = 4
    넘어진확인 = 0

        SPEED 4

        MOVE G6A, 88,  74, 144,  95, 110
        MOVE G6D,108,  76, 146,  93,  96
        MOVE G6B,100
        MOVE G6C,100
        WAIT

        SPEED 8

        MOVE G6A, 90, 90, 120, 105, 110,100
        MOVE G6D,110,  76, 147,  93,  96,100
        MOVE G6B,90
        MOVE G6C,110
        WAIT

    SPEED 보행속도

    MOVE G6A, 86,  56, 145, 115, 110
    MOVE G6D,108,  76, 147,  93,  96
    WAIT


    SPEED 좌우속도
   

    MOVE G6A,110,  76, 147, 93,  96
    MOVE G6D,86, 100, 145,  69, 110
    WAIT

    '*********************************************'
    SPEED 보행속도

    GOSUB 앞뒤기울기측정
	  WAIT


    MOVE G6A,110,  76, 147,  93, 96,100
    MOVE G6D,90, 90, 120, 105, 110,100
    MOVE G6B,110
    MOVE G6C,90
    WAIT

    

    SPEED 보행속도

    MOVE G6D, 86,  56, 145, 115, 110
    MOVE G6A,108,  76, 147,  93,  96
    WAIT

    SPEED 좌우속도
    MOVE G6D,110,  76, 147, 93,  96
    MOVE G6A,86, 100, 145,  69, 110
    WAIT
    '*********************************************'
    SPEED 보행속도

    GOSUB 앞뒤기울기측정
 
	WAIT

    MOVE G6A,90, 90, 120, 105, 110,100
    MOVE G6D,110,  76, 146,  93,  96,100
    MOVE G6B, 90
    MOVE G6C,110
    WAIT
	SPEED 4
	
	MOVE G6A,100,  76, 145,  93, 100, 100
    MOVE G6D,100,  76, 145,  93, 100, 100
    MOVE G6B,100,  30,  80,
    MOVE G6C,100,  30,  80
    WAIT
    
      

    

    RETURN


    '*******************************
	
	RETURN
	'*********************************
전진:
    GOSUB All_motor_mode3
    보행COUNT = 0
    SPEED 7
    HIGHSPEED SETON

    IF 보행순서 = 0 THEN
        보행순서 = 1
        MOVE G6A,95,  76, 147,  93, 101
        MOVE G6D,101,  76, 147,  93, 98
        MOVE G6B,100
        MOVE G6C,100
        WAIT

        GOTO 전진1
    ELSE
        보행순서 = 0
        MOVE G6D,95,  76, 147,  93, 101
        MOVE G6A,101,  76, 147,  93, 98
        MOVE G6B,100
        MOVE G6C,100
        WAIT

        GOTO 전진4
    ENDIF

    '**********************

전진1:
    MOVE G6A,95,  90, 125, 100, 104
    MOVE G6D,104,  77, 147,  93,  102
    MOVE G6B, 85
    MOVE G6C,115
    WAIT

전진2:

    MOVE G6A,103,   73, 140, 103,  100
    MOVE G6D, 95,  85, 147,  85, 102
    WAIT

    GOSUB 앞뒤기울기측정
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0

        RETURN
    ENDIF

    보행COUNT = 보행COUNT + 1
    IF 보행COUNT > 보행횟수 THEN  GOTO 전진2_stop

	GOTO 전진4

    'ERX 4800,A, 전진4
    'IF A <> A_old THEN
전진2_stop:
        MOVE G6D,95,  90, 125, 95, 104
        MOVE G6A,104,  76, 145,  91,  102
        MOVE G6C, 100
        MOVE G6B,100
        WAIT
        HIGHSPEED SETOFF
        SPEED 15
        GOSUB 안정화자세
        SPEED 5
        GOSUB 기본자세2

        'DELAY 400
        RETURN
    'ENDIF

    '*********************************

전진4:
    MOVE G6D,95,  95, 120, 100, 104
    MOVE G6A,104,  77, 147,  93,  102
    MOVE G6C, 85
    MOVE G6B,115
    WAIT

전진5:
    MOVE G6D,103,    73, 140, 103,  100
    MOVE G6A, 95,  85, 147,  85, 102
    WAIT

    GOSUB 앞뒤기울기측정
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0
        RETURN
    ENDIF

    보행COUNT = 보행COUNT + 1
    IF 보행COUNT > 보행횟수 THEN  GOTO 전진5_stop

	GOTO 전진1

    'ERX 4800,A, 전진1
    'IF A <> A_old THEN
전진5_stop:
        MOVE G6A,95,  90, 125, 95, 104
        MOVE G6D,104,  76, 145,  91,  102
        MOVE G6B, 100
        MOVE G6C,100
        WAIT
        HIGHSPEED SETOFF
        SPEED 15
        GOSUB 안정화자세
        SPEED 5
        GOSUB 기본자세2

        'DELAY 400
        RETURN
    'ENDIF

    '*************************************

    '*********************************

    GOTO 전진1

    '****************************************
동쪽동쪽소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 0 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 0 !"

    GOSUB SOUND_PLAY_CHK
    WAIT

    RETURN

    '*********************************
서쪽서쪽소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 1 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 1 !"

    GOSUB SOUND_PLAY_CHK
    WAIT

    RETURN

    '**********************************
남쪽남쪽소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 2 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 2 !"

    GOSUB SOUND_PLAY_CHK
    WAIT

    RETURN
    '***********************************
북쪽북쪽소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 3 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 3 !"

    GOSUB SOUND_PLAY_CHK

    RETURN
    '****************************************
위험지역소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 6 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 6 !"

    GOSUB SOUND_PLAY_CHK
    WAIT

    RETURN
    '****************************
 '****************************
A소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 8 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 8 !"

    GOSUB SOUND_PLAY_CHK
    
    WAIT

    RETURN
    '***************************
B소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 9 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 9 !"

    GOSUB SOUND_PLAY_CHK
    
    WAIT

    RETURN
    '***************************
 '****************************
C소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 10 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 10 !"

    GOSUB SOUND_PLAY_CHK
    
    WAIT

    RETURN
    '***************************
D소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 11 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 11 !"

    GOSUB SOUND_PLAY_CHK
    
    WAIT

    RETURN
    '**************************
구조요청소리내기:
    PRINT "open 22GongMo.mrs !"

    PRINT "SND 7 !"

    GOSUB SOUND_PLAY_CHK

    PRINT "SND 7 !"

    GOSUB SOUND_PLAY_CHK

    RETURN

   ' *******************************************
양손앞으로나란히들기:
    MOVE G6A, 100,  76, 145,  93, 100,
    MOVE G6D, 100,  76, 145,  93, 100,
    MOVE G6B, 190,  20,  80,  ,  ,
    MOVE G6C, 190,  21,  80,  ,  ,
    WAIT
    DELAY 500
    RETURN
    '************************
양손뒤로나란히들기:
    MOVE G6A, 100,  76, 145,  93, 100,
    MOVE G6D, 100,  76, 145,  93, 100,
    MOVE G6B,  10,  30,  80,  ,  ,
    MOVE G6C,  10,  30,  80,  ,  ,
    WAIT
    DELAY 500
    RETURN
   ' ***********************************
오른팔앞으로들기:
    MOVE G6A, 100,  76, 145,  93, 100,
    MOVE G6D, 100,  76, 145,  93, 100,
    MOVE G6B, 100,  30,  80,  ,  ,
    MOVE G6C, 183,  30,  80,  ,  ,
    WAIT
    DELAY 500
    RETURN
   ' ***************************************
왼팔앞으로들기:
    MOVE G6A, 100,  76, 145,  93, 100,
    MOVE G6D, 100,  76, 145,  93, 100,
    MOVE G6B, 185,  30,  80,  ,  ,
    MOVE G6C, 100,  30,  80,  ,  ,
    WAIT
    DELAY 500
    RETURN
   ' *******************************
팔제자리:
    MOVE G6A,100,  76, 145,  93, 100, 100
    MOVE G6D,100,  76, 145,  93, 100, 100
    MOVE G6B,100,  30,  80,
    MOVE G6C,100,  30,  80
    WAIT
    DELAY 500

    mode = 0
    WAIT
    RETURN
   ' ***************************
만세:
    SPEED 5
    MOVE G6A, 100,  76, 145,  93, 100,
    MOVE G6D, 100,  76, 145,  93, 100,
    MOVE G6B, 100, 170,  80,  ,  ,
    MOVE G6C, 100, 170,  80,  ,  ,
    WAIT
    DELAY 500

    RETURN
    '*****************************************
고개동작:

    SPEED 3

    SERVO 16, UPDOWN
    
    WAIT

    RETURN
    
    
        '*************************************
고개좌우:

    SPEED 3

    SERVO 11, ARROW
    
    WAIT

    RETURN
    
    
        '******************************************
좌회전종치기:
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2

    SPEED 10
    MOVE G6A,95,  106, 145,  63, 105, 100
    MOVE G6D,95,  46, 145,  123, 105, 100
    WAIT

    SPEED 12
    MOVE G6A,93,  106, 145,  63, 105, 100
    MOVE G6D,93,  46, 145,  123, 105, 100
    WAIT

    SPEED 8
    WAIT
    MOVE G6A, 100,  76, 145,  93, 100,  
	MOVE G6D, 100,  76, 145,  93, 100,  
	MOVE G6B, 100,  30,  80,  ,  ,  
	MOVE G6C, 100, 190,  80,  ,  ,  
	WAIT
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 10
    MOVE G6A,95,  106, 145,  63, 105, 100
    MOVE G6D,95,  46, 145,  123, 105, 100
    WAIT

    SPEED 12
    MOVE G6A,93,  106, 145,  63, 105, 100
    MOVE G6D,93,  46, 145,  123, 105, 100
    WAIT

    SPEED 8
    WAIT
    MOVE G6A, 100,  76, 145,  93, 100,  
	MOVE G6D, 100,  76, 145,  93, 100,  
	'MOVE G6B, 100,  30,  80,  ,  ,  
	'MOVE G6C, 100, 190,  80,  ,  ,  
	WAIT
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 10
    MOVE G6A,95,  106, 145,  63, 105, 100
    MOVE G6D,95,  46, 145,  123, 105, 100
    WAIT

    SPEED 12
    MOVE G6A,93,  106, 145,  63, 105, 100
    MOVE G6D,93,  46, 145,  123, 105, 100
    WAIT



    SPEED 8
    GOSUB 기본자세2
    WAIT

	DELAY 500
    RETURN
    '******************************************
우회전종치기:
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2

    SPEED 10
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT

    SPEED 12
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 8
    WAIT
    MOVE G6A, 100,  76, 145,  93, 100,  
	MOVE G6D, 100,  76, 145,  93, 100,  
	MOVE G6B, 100,  190,  80,  ,  ,  
	MOVE G6C, 100, 30,  80,  ,  ,  
	WAIT
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 10
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT

    SPEED 12
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 8
    WAIT
    MOVE G6A, 100,  76, 145,  93, 100,  
	MOVE G6D, 100,  76, 145,  93, 100,  
	'MOVE G6B, 100,  190,  80,  ,  ,  
	'MOVE G6C, 100, 30,  80,  ,  , 
	WAIT 
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 10
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT

    SPEED 12
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT



    SPEED 8
    GOSUB 기본자세2
    WAIT
	DELAY 500
    RETURN
    '******************************************
    '*************************************************************************
시민놓기:
    GOSUB All_motor_mode3
    GOSUB 자이로OFF
    MOVE G6B, 145, , ,
    MOVE G6C, 145, , ,
    WAIT

    MOVE G6B, 140, 10, 60,	  ,	  ,
    MOVE G6C, 140, 10, 60,	  ,   ,
    WAIT

    GOSUB 물건놓기_2	

		RETURN

물건놓기_2:
    MOVE G6A, 100, 150, 30,   150, 100,	
    MOVE G6D, 100, 150, 30,   150, 100,
    MOVE G6B, 150, , ,
    MOVE G6C, 150, , ,
    WAIT

    GOSUB 물건놓기_3

		RETURN

물건놓기_3:
    MOVE G6B, , 30, 90
    MOVE G6C, , 30, 90
    WAIT

    GOSUB 물건놓기_4

		RETURN

물건놓기_4:
    MOVE G6A,100,  76, 145,  93, 100, 100
    MOVE G6D,100,  76, 145,  93, 100, 100
    WAIT

    GOSUB 기본자세
    DELAY 500
    GOSUB 자이로ON
    RETURN
    '**************************************************

   ' *********************************
    '*****************'
계단오른발오르기2cm:
    GOSUB All_motor_mode3
    GOSUB All_motor_mode3

    MOVE G6B,100,40
    MOVE G6C,100,100,80
    WAIT

    SPEED 2
    MOVE G6D, 92,  71, 152,  91, 112
    MOVE G6A,116,  77, 146,  93,  94
    MOVE G6B,100,40
    MOVE G6C,100,100,80
    WAIT
    DELAY 1000

    SPEED 6
    MOVE G6D, 92, 100, 110, 100, 116
    MOVE G6A,116,  78, 146,  93,  94
    WAIT

    GOSUB Leg_motor_mode2

    SPEED 6
    MOVE G6D, 90, 140, 35, 130, 116
    MOVE G6A,116,  73, 155,  90,  94
    WAIT


    SPEED 11
    MOVE G6D,  80, 55, 130, 140, 116,
    MOVE G6A,116,  72, 155,  90,  94
    WAIT

    GOSUB Leg_motor_mode3

    SPEED 7
    MOVE G6D, 105, 75, 100, 155, 100,
    MOVE G6A,95,  90, 165,  70, 100
    MOVE G6C,160,50
    MOVE G6B,160,40
    WAIT

    SPEED 6
    MOVE G6D, 113, 90, 90, 155,100,
    MOVE G6A,95,  100, 165,  65, 105
    MOVE G6C,180,50
    MOVE G6B,180,30
    WAIT

    '****************************
    GOSUB Leg_motor_mode2	
    SPEED 8
    MOVE G6D, 114, 90, 100, 150,95,
    MOVE G6A,95,  90, 165,  70, 105
    WAIT

    SPEED 12
    MOVE G6D, 114, 90, 100, 150,95,
    MOVE G6A,90,  120, 40,  140, 108
    WAIT

    SPEED 10
    MOVE G6D, 114, 90, 110, 130,95,
    MOVE G6A,90,  95, 90,  145, 108
    MOVE G6C,140,50
    MOVE G6B,140,30
    WAIT

    SPEED 10
    MOVE G6D, 110, 90, 110, 130,95,
    MOVE G6A,80,  85, 110,  135, 108
    MOVE G6B,110,40
    MOVE G6C,110,40
    WAIT

    SPEED 5
    MOVE G6A, 98, 90, 110, 125,99,
    MOVE G6D,98,  90, 110,  125, 99
    MOVE G6B,110,40
    MOVE G6C,110,40
    WAIT

    SPEED 6
    MOVE G6A,100,  77, 145,  93, 100, 100
    MOVE G6D,100,  77, 145,  93, 100, 100
    MOVE G6B,100,  30,  80
    MOVE G6C,100,  30,  80
    WAIT

    RETURN
    '********************************************
        '************************************************
후진:
    넘어진확인 = 0
    보행속도 = 12
    좌우속도 = 4
    GOSUB Leg_motor_mode3





    SPEED 4
    MOVE G6A, 88,  71, 152,  91, 110
    MOVE G6D,108,  76, 145,  93,  96
    MOVE G6B,100
    MOVE G6C,100
    WAIT

    SPEED 10
    MOVE G6A, 90, 100, 100, 115, 110
    MOVE G6D,110,  76, 145,  93,  96
    MOVE G6B,90
    MOVE G6C,110
    WAIT


    SPEED 보행속도

    MOVE G6D,110,  76, 145, 93,  96
    MOVE G6A,90, 98, 145,  69, 110
    WAIT

    SPEED 좌우속도
    MOVE G6D, 90,  60, 137, 120, 110
    MOVE G6A,107,  85, 137,  93,  96
    WAIT





    SPEED 11

    MOVE G6D,90, 90, 120, 105, 110
    MOVE G6A,112,  76, 146,  93, 96
    MOVE G6B,110
    MOVE G6C,90
    WAIT



    ETX 4800,12 '진행코드를 보냄
    SPEED 보행속도
    MOVE G6A,110,  76, 145, 93,  96
    MOVE G6D,90, 98, 145,  69, 110
    WAIT


    SPEED 좌우속도
    MOVE G6A, 90,  60, 137, 120, 110
    MOVE G6D,107  85, 137,  93,  96
    WAIT


    GOSUB 앞뒤기울기측정
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0
        GOTO MAIN
    ENDIF


    SPEED 11
    MOVE G6A,90, 90, 120, 105, 110
    MOVE G6D,112,  76, 146,  93,  96
    MOVE G6B, 90
    MOVE G6C,110
    WAIT


    HIGHSPEED SETOFF
    SPEED 5

    MOVE G6D, 106,  76, 145,  93,  96		
    MOVE G6A,  85,  72, 148,  91, 106
    MOVE G6B, 100
    MOVE G6C, 100
    WAIT	

    SPEED 3
    GOSUB 기본자세2
    RETURN

    '**********************************************
    '******************************************
계단걸음:
    GOSUB All_motor_mode3
    보행COUNT = 0
    SPEED 7
    HIGHSPEED SETON



    MOVE G6A,95,  76, 147,  93, 101
    MOVE G6D,101,  76, 147,  93, 98
    MOVE G6B,100
    MOVE G6C,100
    WAIT


    MOVE G6A,95,  90, 125, 100, 104
    MOVE G6D,104,  77, 147,  93,  102
    MOVE G6B, 85
    MOVE G6C,115
    WAIT




    MOVE G6A,103,   73, 140, 103,  100
    MOVE G6D, 95,  85, 147,  85, 102
    WAIT

    MOVE G6D,95,  90, 125, 95, 104
    MOVE G6A,104,  76, 145,  91,  102
    MOVE G6C, 100
    MOVE G6B,100
    WAIT
    HIGHSPEED SETOFF
    SPEED 15
    GOSUB 안정화자세
    SPEED 5
    GOSUB 기본자세2

    'DELAY 400
    RETURN

    '******************************************
    '********************************************
왼쪽횡으로이동:
    
        MOTORMODE G6A,3,3,3,3,2
        MOTORMODE G6D,3,3,3,3,2

        SPEED 12
        MOVE G6A, 95,  90, 125, 100, 104, 100
        MOVE G6D,105,  76, 145,  93, 104, 100
        WAIT

        SPEED 12
        MOVE G6A, 102,  77, 145, 93, 100, 100
        MOVE G6D,90,  80, 140,  95, 107, 100
        WAIT

        SPEED 10
        MOVE G6A,95,  76, 145,  93, 102, 100
        MOVE G6D,95,  76, 145,  93, 102, 100
        WAIT

        SPEED 8
        GOSUB 기본자세2
        GOSUB All_motor_mode3
        WAIT
    
    DELAY 500
    

    RETURN
    '********************************
오른쪽횡으로이동:

        MOTORMODE G6A,3,3,3,3,2
        MOTORMODE G6D,3,3,3,3,2

        SPEED 12
        MOVE G6D, 95,  90, 125, 100, 104, 100
        MOVE G6A,105,  76, 146,  93, 104, 100
        WAIT

        SPEED 12
        MOVE G6D, 102,  77, 145, 93, 100, 100
        MOVE G6A,90,  80, 140,  95, 107, 100
        WAIT

        SPEED 10
        MOVE G6D,95,  76, 145,  93, 102, 100
        MOVE G6A,95,  76, 145,  93, 102, 100
        WAIT

        SPEED 8
        GOSUB 기본자세2
        GOSUB All_motor_mode3
        WAIT
    
    WAIT
	DELAY 500
    RETURN
   ' ******************************************
   좌회전30도:
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2


    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
    DELAY 300

    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    
    RETURN
            '**************************
좌회전120도:
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2


    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
    DELAY 300

    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
   	DELAY 300

    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    
    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
    DELAY 300

    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
   	DELAY 300

    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    RETURN
        '**************************
좌회전60도:
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2


    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
    DELAY 300

    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
   	DELAY 300

    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    RETURN
    
    '**************************
좌회전90도:
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2


    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
    DELAY 300

    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
   	DELAY 300

    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  106, 145,  68, 105, 100
    MOVE G6D,95,  46, 145,  118, 105, 100
    WAIT
	DELAY 300
    SPEED 10
    MOVE G6A,93,  106, 145,  68, 105, 100
    MOVE G6D,93,  46, 145,  118, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2

	DELAY 500
    RETURN
   ' ***************************************
좌회전10도:
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2
    SPEED 5
    MOVE G6A,97,  86, 145,  83, 103, 100
    MOVE G6D,97,  66, 145,  103, 103, 100
    WAIT

    SPEED 12
    MOVE G6A,94,  86, 145,  83, 101, 100
    MOVE G6D,94,  66, 145,  103, 101, 100
    WAIT

    SPEED 6
    MOVE G6A,101,  76, 146,  93, 98, 100
    MOVE G6D,101,  76, 146,  93, 98, 100
    WAIT

    GOSUB 기본자세2
	DELAY 500
    RETURN
   ' ************************************
우회전10도:
	MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2
    SPEED 5
    MOVE G6A,97,  66, 145,  103, 103, 100
    MOVE G6D,97,  86, 145,  83, 103, 100
    WAIT

    SPEED 12
    MOVE G6A,94,  66, 145,  103, 101, 100
    MOVE G6D,94,  86, 145,  83, 101, 100
    WAIT

    SPEED 6
    MOVE G6A,101,  76, 146,  93, 98, 100
    MOVE G6D,101,  76, 146,  93, 98, 100
    WAIT

    GOSUB 기본자세2
	DELAY 500
    
	RETURN
		   ' *********************
우회전120도:

    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2


    SPEED 8
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT
	DELAY 200
    SPEED 10
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT
	DELAY 200
    SPEED 10
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT
    DELAY 500
    SPEED 8
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT
	DELAY 200
    SPEED 10
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT
	DELAY 200
    SPEED 10
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    RETURN
	   ' *********************
우회전60도:

    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2


    SPEED 8
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT
	DELAY 200
    SPEED 10
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT
	DELAY 200
    SPEED 10
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    RETURN
   ' *********************
우회전90도:

    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2


    SPEED 8
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT
	DELAY 200
    SPEED 10
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT
	DELAY 200
    SPEED 10
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2
    WAIT

    DELAY 500

    SPEED 8
    MOVE G6A,95,  46, 145,  123, 105, 100
    MOVE G6D,95,  106, 145,  63, 105, 100
    WAIT
	DELAY 200
    SPEED 10
    MOVE G6A,93,  46, 145,  123, 105, 100
    MOVE G6D,93,  106, 145,  63, 105, 100
    WAIT

    SPEED 6
    GOSUB 기본자세2

	WAIT
   
    
    RETURN
        '************************************************
오른쪽옆으로20: '****
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2

    SPEED 12
    MOVE G6D, 95,  90, 125, 100, 104, 100
    MOVE G6A,105,  76, 146,  93, 104, 100
    WAIT

    SPEED 12
    MOVE G6D, 102,  77, 145, 93, 100, 100
    MOVE G6A,90,  80, 140,  95, 107, 100
    WAIT

    SPEED 10
    MOVE G6D,95,  76, 145,  93, 102, 100
    MOVE G6A,95,  76, 145,  93, 102, 100
    WAIT

    SPEED 8
    GOSUB 기본자세2
    GOSUB All_motor_mode3
    WAIT
    DELAY 500
    RETURN
    '*************

왼쪽옆으로20: 
    MOTORMODE G6A,3,3,3,3,2
    MOTORMODE G6D,3,3,3,3,2

    SPEED 12
    MOVE G6A, 95,  90, 125, 100, 104, 100
    MOVE G6D,105,  76, 145,  93, 104, 100
    WAIT

    SPEED 12
    MOVE G6A, 102,  77, 145, 93, 100, 100
    MOVE G6D,90,  80, 140,  95, 107, 100
    WAIT

    SPEED 10
    MOVE G6A,95,  76, 145,  93, 102, 100
    MOVE G6D,95,  76, 145,  93, 102, 100
    WAIT

    SPEED 8
    GOSUB 기본자세2
    GOSUB All_motor_mode3
    WAIT
    DELAY 500
    RETURN
    '**********

시민잡기:
    GOSUB All_motor_mode3
    GOSUB 자이로OFF
    SPEED 8  'before 2
    MOVE G6A, 100, 150, 30,   150, 100,	
    MOVE G6D, 100, 150, 30,   150, 100,
    MOVE G6B, 140, 30, 80,	  ,	  ,
    MOVE G6C, 140, 30, 80,	  ,   ,
    WAIT

    DELAY 50
    GOSUB 물건집기_2

		RETURN

물건집기_2:
    SPEED 4  'before 4
    'DELAY 20
    MOVE G6A, 100, 150, 30,   150, 100,	
    MOVE G6D, 100, 150, 30,   150, 100,
    MOVE G6B, 150, 10, 60,	  ,	  ,
    MOVE G6C, 150, 10, 60,	  ,   ,
    WAIT

    DELAY 20
    GOSUB 물건집기_3

		RETURN

물건집기_3:
    SPEED 8   'before 4
    MOVE G6A,100, 76,  145,    93,  100, 100
    MOVE G6D,100, 76,  145,    93,  100, 100
    MOVE G6B, 150, 10, 60,	  ,	  ,
    MOVE G6C, 150, 10, 60,	  ,   ,
    WAIT

    'DELAY 20
    GOSUB 물건집기_4

		RETURN

물건집기_4:
    SPEED 8  'before 4
    MOVE G6A,100, 76,  145,    93,  100, 100
    MOVE G6D,100, 76,  145,    93,  100, 100
    MOVE G6B, 160
    MOVE G6C, 160
    WAIT
    DELAY 500
	GOSUB 자이로ON

	RETURN

    '*****************
시민잡고전진:
    GOSUB All_motor_mode3
    보행COUNT = 0
    SPEED 7
    HIGHSPEED SETON

    IF 보행순서 = 0 THEN
        보행순서 = 1
        MOVE G6A,95,  76, 147,  93, 101
        MOVE G6D,101,  76, 147,  93, 98

        WAIT

        GOSUB 시민잡고전진1
    ELSE
        보행순서 = 0
        MOVE G6D,95,  76, 147,  93, 101
        MOVE G6A,101,  76, 147,  93, 98

        WAIT

        GOSUB 시민잡고전진4
    ENDIF

		RETURN

    '**********************

시민잡고전진1:
    MOVE G6A,95,  90, 125, 100, 104
    MOVE G6D,104,  77, 147,  93,  102

    WAIT

		GOSUB 시민잡고전진2

		RETURN

시민잡고전진2:

    MOVE G6A,103,   73, 140, 103,  100
    MOVE G6D, 95,  85, 147,  85, 102
    WAIT

    GOSUB 앞뒤기울기측정
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0

        ' RETURN
    ENDIF

    보행COUNT = 보행COUNT + 1
    IF 보행COUNT > 보행횟수 THEN
			GOSUB 시민잡고전진2_stop

		ELSE
			GOSUB 시민잡고전진4

		ENDIF

		RETURN

시민잡고전진2_stop:
        MOVE G6D,95,  90, 125, 95, 104
        MOVE G6A,104,  76, 145,  91,  102

        WAIT
				HIGHSPEED SETOFF

        'DELAY 400
        RETURN

    '*********************************

시민잡고전진4:
    MOVE G6D,95,  95, 120, 100, 104
    MOVE G6A,104,  77, 147,  93,  102

    WAIT

		GOSUB 시민잡고전진5

		RETURN

시민잡고전진5:
    MOVE G6D,103,    73, 140, 103,  100
    MOVE G6A, 95,  85, 147,  85, 102
    WAIT

    GOSUB 앞뒤기울기측정
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0
        ' RETURN
    ENDIF

    보행COUNT = 보행COUNT + 1
    IF 보행COUNT > 보행횟수 THEN
			GOSUB 시민잡고전진5_stop

		ELSE
			GOSUB 시민잡고전진1

		ENDIF

    RETURN
    
시민잡고전진5_stop:
        MOVE G6A,95,  90, 125, 95, 104
        MOVE G6D,104,  76, 145,  91,  102

        WAIT
        HIGHSPEED SETOFF

				RETURN
       
        
        

        DELAY 400
        RETURN
    

    '*************************************

    '*********************************

    GOTO 시민잡고전진1

'*************************************************************************
    '**********************************************

    보행COUNT = 0
    보행속도 = 10
    좌우속도 = 3
    넘어진확인 = 0

    GOSUB Leg_motor_mode3



    SPEED 4

    MOVE G6A, 88,  74, 144,  95, 110
    MOVE G6D,108,  76, 146,  93,  96
    MOVE G6B,100
    MOVE G6C,100
    WAIT

    SPEED 10'

    MOVE G6A, 90, 90, 120, 105, 110,100
    MOVE G6D, 110,  76, 147,  93,  96,100
    MOVE G6B,90
    MOVE G6C,110
    WAIT


    SPEED 보행속도

    MOVE G6A, 86,  56, 145, 115, 110
    MOVE G6D,108,  76, 147,  93,  96
    WAIT


    SPEED 좌우속도
    GOSUB Leg_motor_mode3

    MOVE G6A,110,  76, 147, 93,  96
    MOVE G6D,86, 100, 145,  69, 110
    WAIT


    SPEED 보행속도

    GOSUB 앞뒤기울기측정
    IF 넘어진확인 = 1 THEN
        넘어진확인 = 0
        GOTO MAIN
    ENDIF





    MOVE G6A,105,  76, 147,  93, 96,100
    MOVE G6D,91, 90, 120, 105, 110,100
    MOVE G6B,110
    MOVE G6C,90
    WAIT



    WAIT	
    SPEED 6
    GOSUB 기본자세2

    RETURN


    '*******************************
'*************************************************************************
'*************************** 기본 제공 label *******************************
'*************************************************************************
'*************************************************************************

MOTOR_ON:
    GOSUB MOTOR_GET

    MOTOR G6B
    DELAY 50
    MOTOR G6C
    DELAY 50
    MOTOR G6A
    DELAY 50
    MOTOR G6D

    모터ONOFF = 0
    GOSUB 시작음

    RETURN
    '************************************************
MOTOR_GET:
    GETMOTORSET G6A,1,1,1,1,1,0
    GETMOTORSET G6B,1,1,1,0,0,1
    GETMOTORSET G6C,1,1,1,0,1,0
    GETMOTORSET G6D,1,1,1,1,1,0

    RETURN
    '************************************************
All_motor_Reset:
    MOTORMODE G6A,1,1,1,1,1,1
    MOTORMODE G6D,1,1,1,1,1,1
    MOTORMODE G6B,1,1,1,,,1
    MOTORMODE G6C,1,1,1,,1

    RETURN
    '************************************************
Leg_motor_mode2:
    MOTORMODE G6A,2,2,2,2,2
    MOTORMODE G6D,2,2,2,2,2

    RETURN
    '************************************************
Leg_motor_mode3:
    MOTORMODE G6A,3,3,3,3,3
    MOTORMODE G6D,3,3,3,3,3

    RETURN
    '************************************************
All_motor_mode2:

    MOTORMODE G6A,2,2,2,2,2
    MOTORMODE G6D,2,2,2,2,2
    MOTORMODE G6B,2,2,2,,,2
    MOTORMODE G6C,2,2,2,,2

    RETURN
    '************************************************
All_motor_mode3:

    MOTORMODE G6A,3,3,3,3,3
    MOTORMODE G6D,3,3,3,3,3
    MOTORMODE G6B,3,3,3,,,3
    MOTORMODE G6C,3,3,3,,3

    RETURN
    '************************************************
전원초기자세:
    MOVE G6A,100,  76, 145,  93, 100, 100
    MOVE G6D,100,  76, 145,  93, 100, 100
    MOVE G6B,100,  35,  90,
    MOVE G6C,100,  35,  90
    WAIT

    mode = 0

    RETURN
    '************************************************
자이로INIT:

    GYRODIR G6A, 0, 0, 1, 0,0
    GYRODIR G6D, 1, 0, 1, 0,0

    GYROSENSE G6A,200,150,30,150,0
    GYROSENSE G6D,200,150,30,150,0

    RETURN
    '***********************************************
    '**** 자이로감도 설정 ****
자이로MAX:
    GYROSENSE G6A,250,180,30,180,0
    GYROSENSE G6D,250,180,30,180,0

    RETURN
    '***********************************************
자이로MID:
    GYROSENSE G6A,200,150,30,150,0
    GYROSENSE G6D,200,150,30,150,0

    RETURN
    '***********************************************
자이로MIN:
    GYROSENSE G6A,200,100,30,100,0
    GYROSENSE G6D,200,100,30,100,0

    RETURN
    '***********************************************
자이로ON:
    GYROSET G6A, 4, 3, 3, 3, 0
    GYROSET G6D, 4, 3, 3, 3, 0

    자이로ONOFF = 1

    RETURN
    '***********************************************
자이로OFF:
    GYROSET G6A, 0, 0, 0, 0, 0
    GYROSET G6D, 0, 0, 0, 0, 0

    자이로ONOFF = 0

    RETURN
    '************************************************
시작음:
    TEMPO 220
    MUSIC "O23EAB7EA>3#C"
    RETURN
    '************************************************
기본자세2:
    MOVE G6A,100,  76, 145,  93, 100, 100
    MOVE G6D,100,  76, 145,  93, 100, 100
    MOVE G6B,100,  30,  80,
    MOVE G6C,100,  30,  80
    WAIT

    mode = 0
    RETURN
    '*************************************************
종료음:
    TEMPO 220
    MUSIC "O38GD<BGD<BG"
    RETURN
    '************************************************
에러음:
    TEMPO 250
    MUSIC "FFF"
    RETURN

    '************************************************
안정화자세:
    MOVE G6A,98,  76, 145,  93, 101, 100
    MOVE G6D,98,  76, 145,  93, 101, 100
    MOVE G6B,100,  35,  90,
    MOVE G6C,100,  35,  90
    WAIT
    mode = 0

    RETURN
    '************************************************

NUM_TO_ARR:
    NO_4 =  BUTTON_NO / 10000
    TEMP_INTEGER = BUTTON_NO MOD 10000

    NO_3 =  TEMP_INTEGER / 1000
    TEMP_INTEGER = BUTTON_NO MOD 1000

    NO_2 =  TEMP_INTEGER / 100
    TEMP_INTEGER = BUTTON_NO MOD 100

    NO_1 =  TEMP_INTEGER / 10
    TEMP_INTEGER = BUTTON_NO MOD 10

    NO_0 =  TEMP_INTEGER

    RETURN
    '************************************************
Number_Play:
    GOSUB NUM_TO_ARR

    PRINT "NPL "
    '*************
    NUM = NO_4
    GOSUB NUM_1_9
    '*************
    NUM = NO_3
    GOSUB NUM_1_9
    '*************
    NUM = NO_2
    GOSUB NUM_1_9
    '*************
    NUM = NO_1
    GOSUB NUM_1_9
    '*************
    NUM = NO_0
    GOSUB NUM_1_9
    PRINT " !"

    GOSUB SOUND_PLAY_CHK
    PRINT "SND 16 !"
    GOSUB SOUND_PLAY_CHK

    RETURN
    '************************************************
SOUND_PLAY_CHK:
    DELAY 60
    SOUND_BUSY = IN(46)
    IF SOUND_BUSY = 1 THEN GOTO SOUND_PLAY_CHK
    DELAY 50

    RETURN
    '************************************************
NUM_1_9:
    IF NUM = 1 THEN
        PRINT "1"
    ELSEIF NUM = 2 THEN
        PRINT "2"
    ELSEIF NUM = 3 THEN
        PRINT "3"
    ELSEIF NUM = 4 THEN
        PRINT "4"
    ELSEIF NUM = 5 THEN
        PRINT "5"
    ELSEIF NUM = 6 THEN
        PRINT "6"
    ELSEIF NUM = 7 THEN
        PRINT "7"
    ELSEIF NUM = 8 THEN
        PRINT "8"
    ELSEIF NUM = 9 THEN
        PRINT "9"
    ELSEIF NUM = 0 THEN
        PRINT "0"
    ENDIF

    RETURN
    '************************************************

    '************************************************
    '************************************************

앞뒤기울기측정:
    FOR i = 0 TO COUNT_MAX
        A = AD(앞뒤기울기AD포트)	'기울기 앞뒤
        IF A > 250 OR A < 5 THEN RETURN
        IF A > MIN AND A < MAX THEN RETURN
        DELAY 기울기확인시간
    NEXT i

    IF A < MIN THEN
        GOSUB 기울기앞
    ELSEIF A > MAX THEN
        GOSUB 기울기뒤
    ENDIF

    RETURN
    '**************************************************

    '**************************************************
적외선거리센서확인:
    적외선거리값 = AD(적외선AD포트)

    IF 적외선거리값 > 50 THEN '50 = 적외선거리값 = 25cm
        MUSIC "C"
        DELAY 200
    ENDIF

    RETURN
    ' ************************************************
기울기앞:
    A = AD(앞뒤기울기AD포트)
    'IF A < MIN THEN GOSUB 앞으로일어나기
    IF A < MIN THEN
        GOSUB 뒤로일어나기
    ENDIF

    RETURN
    '**************************************************
기울기뒤:
    A = AD(앞뒤기울기AD포트)
    'IF A > MAX THEN GOSUB 뒤로일어나기
    IF A > MAX THEN
        GOSUB 앞으로일어나기
    ENDIF

    RETURN
    '**************************************************
앞으로일어나기:
    HIGHSPEED SETOFF
    PTP SETON 				
    PTP ALLON

    GOSUB 자이로OFF

    HIGHSPEED SETOFF

    GOSUB All_motor_Reset

    SPEED 15
    MOVE G6A,100, 15,  70, 140, 100,
    MOVE G6D,100, 15,  70, 140, 100,
    MOVE G6B,20,  140,  15
    MOVE G6C,20,  140,  15
    WAIT

    SPEED 12
    MOVE G6A,100, 136,  35, 80, 100,
    MOVE G6D,100, 136,  35, 80, 100,
    MOVE G6B,20,  30,  80
    MOVE G6C,20,  30,  80
    WAIT

    SPEED 12
    MOVE G6A,100, 165,  70, 30, 100,
    MOVE G6D,100, 165,  70, 30, 100,
    MOVE G6B,30,  20,  95
    MOVE G6C,30,  20,  95
    WAIT

    GOSUB Leg_motor_mode3

    SPEED 10
    MOVE G6A,100, 165,  45, 90, 100,
    MOVE G6D,100, 165,  45, 90, 100,
    MOVE G6B,130,  50,  60
    MOVE G6C,130,  50,  60
    WAIT

    SPEED 6
    MOVE G6A,100, 145,  45, 130, 100,
    MOVE G6D,100, 145,  45, 130, 100,
    WAIT

    SPEED 8
    GOSUB All_motor_mode2
    GOSUB 기본자세
    넘어진확인 = 1

    DELAY 200
    GOSUB 자이로ON

    RETURN
    '***********************************************
뒤로일어나기:
    HIGHSPEED SETOFF
    PTP SETON 				
    PTP ALLON		

    GOSUB 자이로OFF

    GOSUB All_motor_Reset

    SPEED 15
    GOSUB 기본자세

    MOVE G6A,90, 130, ,  80, 110, 100
    MOVE G6D,90, 130, 120,  80, 110, 100
    MOVE G6B,150, 160,  10, 100, 100, 100
    MOVE G6C,150, 160,  10, 100, 100, 100
    WAIT

    MOVE G6B,170, 140,  10, 100, 100, 100
    MOVE G6C,170, 140,  10, 100, 100, 100
    WAIT

    MOVE G6B,185,  20, 70,  100, 100, 100
    MOVE G6C,185,  20, 70,  100, 100, 100
    WAIT

    SPEED 10
    MOVE G6A, 80, 155,  85, 150, 150, 100
    MOVE G6D, 80, 155,  85, 150, 150, 100
    MOVE G6B,185,  20, 70,  100, 100, 100
    MOVE G6C,185,  20, 70,  100, 100, 100
    WAIT

    MOVE G6A, 75, 162,  55, 162, 155, 100
    MOVE G6D, 75, 162,  59, 162, 155, 100
    MOVE G6B,188,  10, 100, 100, 100, 100
    MOVE G6C,188,  10, 100, 100, 100, 100
    WAIT

    SPEED 10
    MOVE G6A, 60, 162,  30, 162, 145, 100
    MOVE G6D, 60, 162,  30, 162, 145, 100
    MOVE G6B,170,  10, 100, 100, 100, 100
    MOVE G6C,170,  10, 100, 100, 100, 100
    WAIT

    GOSUB Leg_motor_mode3	

    MOVE G6A, 60, 150,  28, 155, 140, 100
    MOVE G6D, 60, 150,  28, 155, 140, 100
    MOVE G6B,150,  60,  90, 100, 100, 100
    MOVE G6C,150,  60,  90, 100, 100, 100
    WAIT

    MOVE G6A,100, 150,  28, 140, 100, 100
    MOVE G6D,100, 150,  28, 140, 100, 100
    MOVE G6B,130,  50,  85, 100, 100, 100
    MOVE G6C,130,  50,  85, 100, 100, 100
    WAIT

    DELAY 100

    MOVE G6A,100, 150,  33, 140, 100, 100
    MOVE G6D,100, 150,  33, 140, 100, 100
    WAIT
    SPEED 10
    GOSUB 기본자세

    넘어진확인 = 1

    DELAY 200
    GOSUB 자이로ON

    RETURN

    '************************************************
기본자세:
    MOVE G6A,100,  76, 145,  93, 100, 100
    MOVE G6D,100,  76, 145,  93, 100, 100
    MOVE G6B,100,  30,  80,
    MOVE G6C,100,  30,  80,
    WAIT

    mode = 0

    RETURN