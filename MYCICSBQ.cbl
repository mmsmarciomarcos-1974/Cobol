       IDENTIFICATION DIVISION.
       PROGRAM-ID. MYCICSBQ.
       AUTHOR.     MARCIO MARCOS.
      *
      *================================================================*
      * PROGRAMA CICS - BROWSE (PAGINACAO) COM DB2
      *
      * TRANSACAO: 'TRN4'
      * TABELA:    'TB_CLIENTES'
      * MAPA:      'MYMAP4'
      *
      * LOGICA:
      * 1. EIBCALEN = 0: Carrega a primeira pagina (a partir de LOW-VALUES)
      * 2. EIBAID = PF8: Carrega a proxima pagina
      * 3. EIBAID = PF7: Carrega a pagina anterior (Logica complexa)
      * 4. EIBAID = PF3: Sai
      *================================================================*
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *--- VARIAVEIS DE CONTROLE
       01 WS-PROGRAM-VARS.
           05 WS-MSG-SAIDA        PIC X(70).
           05 WS-SUB              PIC 9(02) COMP. *> Subscrito do loop
           05 WS-MAX-LINHAS       PIC 9(02) COMP VALUE 10.
           05 WS-START-KEY        PIC X(10). *> Chave p/ iniciar o cursor
           05 WS-ROWS-FETCHED     PIC 9(02) COMP.
      *
      *--- SQLCA E DCLGEN
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLCLI END-EXEC.
      *
       01 WS-SQL-CONTROLE.
           05 WS-SQLCODE            PIC S9(09) COMP VALUE 0.
              88 SQL-OK             VALUE 0.
              88 SQL-NOT-FOUND      VALUE +100.
      *
      *--- MAPA SIMBOLICO
       COPY MYMAP4C. *> Assume BMS gerou MYMAP4C com 10 linhas
      *
      *--- AREA DE COMUNICACAO (SALVA O ESTADO DA PAGINACAO)
       01 WS-COMMAREA.
           05 CA-FIRST-KEY-ON-SCREEN PIC X(10). *> 1a chave da tela
           05 CA-LAST-KEY-ON-SCREEN  PIC X(10). *> Ultima chave da tela
           05 CA-FLAG-END-OF-DATA    PIC X(01) VALUE 'N'.
              88 IS-END-OF-DATA                VALUE 'Y'.
           05 CA-FLAG-START-OF-DATA  PIC X(01) VALUE 'N'.
              88 IS-START-OF-DATA              VALUE 'Y'.
      *
      *--- COPYBOOKS PADRAO CICS
       COPY DFHAID.
      *
      *--- DECLARACOES DOS CURSORES DB2
       01 WS-CURSORS-DB2.
      *--- CURSOR 1: PARA FRENTE (PF8 / CARGA INICIAL)
           EXEC SQL
               DECLARE C_FWD CURSOR FOR
                  SELECT ID_CLIENTE, NOME_CLIENTE
                  FROM TB_CLIENTES
                  WHERE ID_CLIENTE > :WS-START-KEY
                  ORDER BY ID_CLIENTE ASC
           END-EXEC.
      *
      *--- CURSOR 2: PARA TRAS (USADO APENAS PELO PF7)
           EXEC SQL
               DECLARE C_BACK CURSOR FOR
                  SELECT ID_CLIENTE
                  FROM TB_CLIENTES
                  WHERE ID_CLIENTE < :WS-START-KEY
                  ORDER BY ID_CLIENTE DESC
           END-EXEC.
      *
      *--- CURSOR 3: CARGA INICIAL (UNICO QUE USA >=)
           EXEC SQL
               DECLARE C_START CURSOR FOR
                  SELECT ID_CLIENTE, NOME_CLIENTE
                  FROM TB_CLIENTES
                  WHERE ID_CLIENTE >= :WS-START-KEY
                  ORDER BY ID_CLIENTE ASC
           END-EXEC.
      *
       LINKAGE SECTION.
      *
       01 DFHEIBLK.
          COPY DFHEIBLK.
       01 DFHCOMMAREA             PIC X(22). *> Tamanho de WS-COMMAREA
      *
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       MAIN-PROCEDURE.
           EXEC CICS HANDLE CONDITION
               ERROR(9000-CICS-ERROR)
               MAPFAIL(9100-MAPFAIL-ERROR)
           END-EXEC.
           EXEC CICS HANDLE ABEND
               PROGRAM(9900-PROGRAM-ABEND)
           END-EXEC.
      *
           EVALUATE EIBCALEN
               WHEN 0
                   PERFORM 1000-FIRST-TIME
               WHEN OTHER
                   PERFORM 2000-SUBSEQUENT-TIME
           END-EVALUATE.
       GOBACK.
      *
      *================================================================*
      * PARAGRAFOS DE PROCESSAMENTO PRINCIPAL
      *================================================================*
      *
       1000-FIRST-TIME.
      * Primeira execucao: Inicia do comeco do arquivo.
           INITIALIZE WS-COMMAREA.
           MOVE LOW-VALUES TO WS-START-KEY.
           SET IS-START-OF-DATA TO TRUE.
           MOVE 'INICIO DA CONSULTA' TO WS-MSG-SAIDA.
      *
           PERFORM 4000-LOAD-DATA-START. *> Unico que usa C_START (>=)
           PERFORM 5000-SEND-AND-RETURN.
           EXIT.
      *
       2000-SUBSEQUENT-TIME.
      * Restaura o estado da sessao
           MOVE DFHCOMMAREA TO WS-COMMAREA.
           MOVE SPACES TO WS-MSG-SAIDA.
      *
           EVALUATE EIBAID
               WHEN DFHPF8 *> PAGE FORWARD
                   PERFORM 2100-PROCESS-PAGE-FORWARD
      *
               WHEN DFHPF7 *> PAGE BACK
                   PERFORM 2200-PROCESS-PAGE-BACK
      *
               WHEN DFHPF3 *> EXIT
                   PERFORM 3000-PROCESS-EXIT
      *
               WHEN OTHER
                   MOVE 'TECLA INVALIDA. USE PF3, PF7 OU PF8.'
                       TO WS-MSG-SAIDA
                   PERFORM 5000-SEND-AND-RETURN
           END-EVALUATE.
           EXIT.
      *