       IDENTIFICATION DIVISION.
       PROGRAM-ID. MYCICS.
       AUTHOR.     MARCIO MARCOS.
      *
      *================================================================*
      * PROGRAMA CICS ON-LINE (PSEUDO-CONVERSACIONAL)
      *
      * TRANSACAO: 'TRN1' (Exemplo)
      * MAPSET:    'MYSET1'
      * MAPA:      'MYMAP1'
      *
      * LOGICA:
      * 1. SE EIBCALEN = 0 (PRIMEIRA VEZ): Envia o mapa limpo.
      * 2. SE EIBCALEN > 0 (RETORNO):
      * - Se apertou ENTER: Recebe dados, processa, envia mapa c/ msg.
      * - Se apertou PF3: Envia mensagem de "adeus" e termina.
      *================================================================*
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *--- VARIAVEIS DE CONTROLE E MENSAGENS
       01 WS-PROGRAM-VARS.
           05 WS-MSG-SAIDA        PIC X(70).
           05 WS-MSG-ADEUS        PIC X(30) VALUE 'Sessao encerrada.'.
      *
      *--- AREA DE COMUNICACAO (PARA "LEMBRAR" DADOS ENTRE EXECUCOES)
       01 WS-COMMAREA.
           05 WS-CA-DADO-SALVO    PIC X(50) VALUE SPACES.
      *
      *--- MAPA SIMBOLICO (GERADO PELO BMS, AQUI SIMULADO)
      *--- NORMALMENTE SERIA: COPY MYMAP1C.
       01 WS-MYMAP1-AREA.
           05 FILLER              PIC X(12). *> DFH-NULL
           05 DATAL               PIC S9(04) COMP.
           05 DATA-FLAG           PIC X(01).
           05 DATAI               PIC X(30). *> CAMPO DE ENTRADA
           05 FILLER              PIC X(12). *> DFH-NULL
           05 MSGOL               PIC S9(04) COMP.
           05 MSGO-FLAG           PIC X(01).
           05 MSGO                PIC X(70). *> CAMPO DE MENSAGEM
      *
      *--- COPYBOOKS PADRAO CICS (PARA AS TECLAS DE ATENCAO)
       COPY DFHAID.
      *
       LINKAGE SECTION.
      *
      *--- BLOCO DE INTERFACE DO CICS (SEMPRE PRESENTE)
       01 DFHEIBLK.
          COPY DFHEIBLK.
      *
      *--- AREA DE COMUNICACAO (COMO O CICS A VE)
       01 DFHCOMMAREA             PIC X(50). *> Deve ser do mesmo tamanho
                                           *> da WS-COMMAREA
      *
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       MAIN-PROCEDURE.
      *
      *--- CONFIGURA O MANIPULADOR DE ERROS PADRAO
           PERFORM 1000-HANDLE-ERRORS.
      *
      *--- O CORACAO DO PROGRAMA: VERIFICA O TAMANHO DA COMMAREA
           EVALUATE EIBCALEN
      *
      * CASO 1: EIBCALEN = 0. E a primeira vez que a transacao roda.
               WHEN 0
                   PERFORM 2000-FIRST-TIME
      *
      * CASO 2: EIBCALEN > 0. O usuario apertou uma tecla.
               WHEN OTHER
                   PERFORM 3000-SUBSEQUENT-TIME
      *
           END-EVALUATE.
      *
      * O STOP RUN NAO E USADO EM CICS. O PROGRAMA TERMINA COM 'RETURN'.
       GOBACK.
      *
      *================================================================*
      * PARAGRAFOS DE PROCESSAMENTO
      *================================================================*
      *
       1000-HANDLE-ERRORS.
      * Define rotinas para lidar com erros graves
           EXEC CICS HANDLE CONDITION
               ERROR(9000-CICS-ERROR)
               MAPFAIL(9100-MAPFAIL-ERROR)
           END-EXEC.
           EXEC CICS HANDLE ABEND
               PROGRAM(9900-PROGRAM-ABEND)
           END-EXEC.
           EXIT.
      *
       2000-FIRST-TIME.
      * Logica para a primeira execucao da transacao.
      * Limpa a Commarea e o Mapa, e envia a tela inicial.
           INITIALIZE WS-COMMAREA.
           INITIALIZE WS-MYMAP1-AREA.
      *
           MOVE 'BEM-VINDO. POR FAVOR, DIGITE UM DADO:' TO MSGO.
      *
      *--- ENVIA O MAPA (TELA) PARA O TERMINAL
           EXEC CICS SEND
               MAP('MYMAP1')         *> Nome do Mapa
               MAPSET('MYSET1')      *> Nome do Mapset
               FROM(WS-MYMAP1-AREA)  *> Area de dados do mapa
               ERASE                 *> Apaga a tela antes de enviar
               MAPONLY               *> Envia apenas o mapa (sem dados)
           END-EXEC.
      *
      *--- TERMINA A TAREFA E ESPERA O USUARIO (PSEUDO-CONVERSACIONAL)
           EXEC CICS RETURN
               TRANSID('TRN1')       *> ID da transacao a re-iniciar
               COMMAREA(WS-COMMAREA) *> Salva a Commarea
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
           EXIT.
      *
       3000-SUBSEQUENT-TIME.
      * Logica para quando o usuario ja esta na tela e aperta algo.
      * Primeiro, restaura o "estado" (Commarea).
           MOVE DFHCOMMAREA TO WS-COMMAREA.
      *
      * Verifica qual tecla foi pressionada (EIBAID vem do DFHEIBLK).
           EVALUATE EIBAID
      *
      * CASO 2A: Usuario apertou ENTER
               WHEN DFHENTER
                   PERFORM 3100-PROCESS-ENTER
      *
      * CASO 2B: Usuario apertou PF3 (Sair)
               WHEN DFHPF3
                   PERFORM 3200-PROCESS-EXIT
      *
      * CASO 2C: Outra tecla (Clear, PA1, etc.)
               WHEN OTHER
                   PERFORM 3300-INVALID-KEY
           END-EVALUATE.
           EXIT.
      *
       3100-PROCESS-ENTER.
      * Usuario apertou ENTER. Devemos ler os dados da tela.
      *
      *--- RECEBE OS DADOS QUE O USUARIO DIGITOU
           EXEC CICS RECEIVE
               MAP('MYMAP1')
               MAPSET('MYSET1')
               INTO(WS-MYMAP1-AREA)
           END-EXEC.
      *
      *--- [ INICIO DA LOGICA DE NEGOCIO ] ---
           MOVE DATAI TO WS-CA-DADO-SALVO. *> Salva o dado na Commarea
           MOVE 'DADO RECEBIDO E SALVO:' TO WS-MSG-SAIDA.
      *--- [ FIM DA LOGICA DE NEGOCIO ] ---
      *
      *--- Prepara a mensagem de resposta
           MOVE SPACES TO MSGO.
           STRING WS-MSG-SAIDA DELIMITED BY SIZE
                  WS-CA-DADO-SALVO DELIMITED BY SIZE
                  INTO MSGO.
      *
      *--- Envia o mapa ATUALIZADO (so os dados) de volta
           EXEC CICS SEND
               MAP('MYMAP1')
               MAPSET('MYSET1')
               FROM(WS-MYMAP1-AREA)
               DATAONLY              *> So atualiza os dados
           END-EXEC.
      *
      *--- Continua a conversacao
           EXEC CICS RETURN
               TRANSID('TRN1')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
           EXIT.
      *
       3200-PROCESS-EXIT.
      * Usuario apertou PF3. Envia uma mensagem limpa e encerra.
           EXEC CICS SEND
               TEXT
               FROM(WS-MSG-ADEUS)
               ERASE                 *> Apaga a tela
           END-EXEC.
      *
      *--- TERMINA A TAREFA (SEM TRANSID = FIM DA SESSAO)
           EXEC CICS RETURN
           END-EXEC.
           EXIT.
      *
       3300-INVALID-KEY.
      * Usuario apertou uma tecla nao mapeada (ex: PF5)
           MOVE 'TECLA INVALIDA. USE ENTER OU PF3.' TO MSGO.
      *
      *--- Re-envia o mapa com a mensagem de erro
           EXEC CICS SEND
               MAP('MYMAP1')
               MAPSET('MYSET1')
               FROM(WS-MYMAP1-AREA)
               DATAONLY
           END-EXEC.
      *
      *--- Continua a conversacao
           EXEC CICS RETURN
               TRANSID('TRN1')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
           EXIT.
      *
      *================================================================*
      * PARAGRAFOS DE ERRO
      *================================================================*
       9000-CICS-ERROR.
           DISPLAY 'ERRO GRAVE DE CICS. FUNCAO: ' EIBFN.
           PERFORM 9999-ABEND-ROUTINE.
           EXIT.
      *
       9100-MAPFAIL-ERROR.
           DISPLAY 'ERRO DE MAPFAIL. DADOS INVALIDOS RECEBIDOS.'.
           PERFORM 3300-INVALID-KEY. *> Trata como tecla invalida
           EXIT.
      *
       9900-PROGRAM-ABEND.
           DISPLAY 'PROGRAM ABEND DETECTADO'.
      * (Aqui poderia gravar um log, etc.)
       9999-ABEND-ROUTINE.
      * Envia mensagem de erro fatal ao usuario e encerra
           EXEC CICS SEND
               TEXT
               FROM('ERRO IRRECUPERAVEL. CONTATE O SUPORTE.')
               ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
           EXIT.