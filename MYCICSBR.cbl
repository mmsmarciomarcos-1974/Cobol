	   IDENTIFICATION DIVISION.
       PROGRAM-ID. MYCICSBR.
       AUTHOR.     MARCIO MARCOS.
      *
      *================================================================*
      * PROGRAMA CICS - BROWSE (CONSULTA COM PAGINACAO)
      *
      * TRANSACAO: 'TRN2' (Exemplo)
      * ARQUIVO:   'CUSTFILE' (VSAM KSDS)
      *
      * LOGICA:
      * 1. EIBCALEN = 0: Carrega a primeira pagina (a partir de LOW-VALUES)
      * 2. EIBAID = PF8: Carrega a proxima pagina
      * 3. EIBAID = PF7: Carrega a pagina anterior
      * 4. EIBAID = PF3: Sai
      *================================================================*
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTFILE ASSIGN TO CUSTFILE
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CUST-ID
                  FILE STATUS IS WS-CUSTFILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTFILE
          RECORD CONTAINS 100 CHARACTERS.
       01 CUST-RECORD.
          05 CUST-ID            PIC X(10).
          05 CUST-NOME          PIC X(40).
          05 FILLER             PIC X(50).

       WORKING-STORAGE SECTION.
      *
      *--- VARIAVEIS DE CONTROLE DE ARQUIVO E LOOP
       01 WS-PROGRAM-VARS.
           05 WS-CUSTFILE-STATUS  PIC X(02).
           05 WS-SUB              PIC 9(02) COMP. *> Subscrito do loop
           05 WS-MAX-LINHAS       PIC 9(02) COMP VALUE 10.
           05 WS-MSG-SAIDA        PIC X(70).
      *
      *--- MAPA SIMBOLICO (GERADO PELO BMS)
       COPY MYMAP2C. *> Assume que o BMS gerou MYMAP2C
      *
      *--- AREA DE COMUNICACAO (PARA SALVAR O ESTADO DA PAGINACAO)
       01 WS-COMMAREA.
           05 CA-MSG              PIC X(70).
           05 CA-FIRST-KEY-SCREEN PIC X(10). *> Chave do 1o reg da tela
           05 CA-LAST-KEY-SCREEN  PIC X(10). *> Chave do ultimo reg da tela
           05 CA-START-BROWSE-KEY PIC X(10). *> Chave p/ proximo STARTBR
      *
      *--- COPYBOOKS PADRAO CICS
       COPY DFHAID.
      *
       LINKAGE SECTION.
      *
       01 DFHEIBLK.
          COPY DFHEIBLK.
       01 DFHCOMMAREA             PIC X(90). *> Tamanho de WS-COMMAREA
      *
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       MAIN-PROCEDURE.
           EXEC CICS HANDLE CONDITION
               ERROR(9000-CICS-ERROR)
               MAPFAIL(9100-MAPFAIL-ERROR)
               NOTFND(9200-NOTFND-ERROR)
               ENDFILE(9300-ENDFILE-ERROR)
           END-EXEC.
           EXEC CICS HANDLE ABEND
               PROGRAM(9900-PROGRAM-ABEND)
           END-EXEC.
      *
      *--- VERIFICA O TAMANHO DA COMMAREA
           EVALUATE EIBCALEN
      *
      * CASO 1: EIBCALEN = 0. PRIMEIRA VEZ.
               WHEN 0
                   PERFORM 2000-FIRST-TIME
      *
      * CASO 2: EIBCALEN > 0. RETORNO DO USUARIO.
               WHEN OTHER
                   PERFORM 3000-SUBSEQUENT-TIME
      *
           END-EVALUATE.
       GOBACK.
      *
      *================================================================*
      * PARAGRAFOS DE PROCESSAMENTO PRINCIPAL
      *================================================================*
      *
       2000-FIRST-TIME.
      * Limpa a Commarea e define a chave inicial (inicio do arquivo)
           INITIALIZE WS-COMMAREA.
           MOVE LOW-VALUES TO CA-START-BROWSE-KEY.
           MOVE 'INICIO DO ARQUIVO' TO WS-MSG-SAIDA.
      *
           PERFORM 4000-LOAD-DATA-FORWARD.
           PERFORM 5000-SEND-AND-RETURN.
           EXIT.
      *
       3000-SUBSEQUENT-TIME.
      * Restaura o estado da sessao
           MOVE DFHCOMMAREA TO WS-COMMAREA.
           MOVE SPACES TO WS-MSG-SAIDA.
      *
           EVALUATE EIBAID
               WHEN DFHPF8 *> PAGE FORWARD
                   PERFORM 3100-PROCESS-PAGE-FORWARD
      *
               WHEN DFHPF7 *> PAGE BACK
                   PERFORM 3200-PROCESS-PAGE-BACK
      *
               WHEN DFHPF3 *> EXIT
                   PERFORM 3300-PROCESS-EXIT
      *
               WHEN OTHER
                   MOVE 'TECLA INVALIDA. USE PF3, PF7 OU PF8.'
                       TO WS-MSG-SAIDA
                   PERFORM 5000-SEND-AND-RETURN
           END-EVALUATE.
           EXIT.
      *
       3100-PROCESS-PAGE-FORWARD.
      * Define a chave inicial como a ULTIMA chave da tela anterior
           MOVE CA-LAST-KEY-SCREEN TO CA-START-BROWSE-KEY.
      *
           PERFORM 4000-LOAD-DATA-FORWARD.
           PERFORM 5000-SEND-AND-RETURN.
           EXIT.
      *
       3200-PROCESS-PAGE-BACK.
      * Se ja estamos na primeira pagina, nao faz nada
           IF CA-FIRST-KEY-SCREEN = LOW-VALUES
              MOVE 'JA ESTA NO INICIO DO ARQUIVO.' TO WS-MSG-SAIDA
              PERFORM 5000-SEND-AND-RETURN
           ELSE
      * Logica complexa: le 10 registros PARA TRAS para achar
      * a chave de inicio da pagina anterior.
              PERFORM 4100-CALCULATE-PAGE-BACK-KEY
              PERFORM 4000-LOAD-DATA-FORWARD
              PERFORM 5000-SEND-AND-RETURN
           END-IF.
           EXIT.
      *
       3300-PROCESS-EXIT.
           EXEC CICS SEND
               TEXT
               FROM('Sessao de consulta encerrada.')
               ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
           EXIT.
      *
      *================================================================*
      * PARAGRAFOS DE I/O (O CORACAO DA LOGICA)
      *================================================================*
      *
       4000-LOAD-DATA-FORWARD.
      * Rotina principal: Carrega 10 registros A PARTIR
      * da chave em CA-START-BROWSE-KEY.
      *
           INITIALIZE WS-MYMAP2-AREA.
           MOVE CA-START-BROWSE-KEY TO CUST-ID.
      *
      *--- INICIA A NAVEGACAO NO ARQUIVO
           EXEC CICS STARTBR
               FILE('CUSTFILE')
               RIDFLD(CUST-ID)
               GENERIC
               GTEQ
           END-EXEC.
      *
      *--- SE ESTAMOS AVANCANDO (PF8), PULAMOS O PRIMEIRO REGISTRO
      *--- (POIS ELE ERA O ULTIMO DA TELA ANTERIOR)
           IF EIBAID = DFHPF8
              EXEC CICS READNEXT
                  FILE('CUSTFILE')
                  INTO(CUST-RECORD)
                  RIDFLD(CUST-ID)
              END-EXEC
           END-IF.
      *
      *--- PREENCHE AS 10 LINHAS DA TELA
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > WS-MAX-LINHAS
      *
              EXEC CICS READNEXT
                  FILE('CUSTFILE')
                  INTO(CUST-RECORD)
                  RIDFLD(CUST-ID)
              END-EXEC
      *
      *--- SE O ARQUIVO ACABOU, SAI DO LOOP
              IF EIBRESP = DFHRESP(ENDFILE)
                 MOVE 'FIM DOS DADOS.' TO WS-MSG-SAIDA
                 EXIT PERFORM
              END-IF
      *
      *--- SALVA A PRIMEIRA CHAVE (APENAS NA PRIMEIRA LINHA)
              IF WS-SUB = 1
                 MOVE CUST-ID TO CA-FIRST-KEY-SCREEN
              END-IF
      *
      *--- SALVA A CHAVE ATUAL COMO A "ULTIMA CHAVE"
              MOVE CUST-ID TO CA-LAST-KEY-SCREEN
      *
      *--- MOVE DADOS PARA A LINHA DO MAPA
              MOVE CUST-ID TO ID-CLIENTEO(WS-SUB)
              MOVE CUST-NOME TO NOME-CLIENTEO(WS-SUB)
      *
           END-PERFORM.
      *
           EXEC CICS ENDBR FILE('CUSTFILE') END-EXEC.
           EXIT.
      *
       4100-CALCULATE-PAGE-BACK-KEY.
      * Rotina para "voltar a pagina".
      * Le 10 registros PARA TRAS para descobrir onde a
      * pagina anterior comecava.
      *
           MOVE CA-FIRST-KEY-SCREEN TO CUST-ID.
      *
           EXEC CICS STARTBR
               FILE('CUSTFILE')
               RIDFLD(CUST-ID)
               GENERIC
               GTEQ
           END-EXEC.
      *
      *--- LE 10 VEZES PARA TRAS
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > WS-MAX-LINHAS
      *
              EXEC CICS READPREV
                  FILE('CUSTFILE')
                  INTO(CUST-RECORD)
                  RIDFLD(CUST-ID)
              END-EXEC
      *
              IF EIBRESP = DFHRESP(ENDFILE) *> Chegou no inicio do arquivo
                 MOVE LOW-VALUES TO CA-START-BROWSE-KEY
                 EXIT PERFORM
              ELSE
                 MOVE CUST-ID TO CA-START-BROWSE-KEY
              END-IF
           END-PERFORM.
      *
           EXEC CICS ENDBR FILE('CUSTFILE') END-EXEC.
           EXIT.
      *
       5000-SEND-AND-RETURN.
      * Envia o mapa populado e retorna para o CICS
      *
           MOVE WS-MSG-SAIDA TO MSGO.
      *
           EXEC CICS SEND
               MAP('MYMAP2')
               MAPSET('MYSET2')
               FROM(WS-MYMAP2-AREA)
               ERASE
           END-EXEC.
      *
           EXEC CICS RETURN
               TRANSID('TRN2')
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
           PERFORM 9900-PROGRAM-ABEND.
           EXIT.
       9100-MAPFAIL-ERROR.
           DISPLAY 'ERRO DE MAPFAIL.'.
           PERFORM 9900-PROGRAM-ABEND.
           EXIT.
       9200-NOTFND-ERROR.
           DISPLAY 'REGISTRO NAO ENCONTRADO NO VSAM.'.
           PERFORM 9900-PROGRAM-ABEND.
           EXIT.
       9300-ENDFILE-ERROR.
      * Esta condicao e tratada no loop (4000-), mas se ocorrer
      * fora dele, e um erro