       IDENTIFICATION DIVISION.
       PROGRAM-ID. MYDB2BAT.
       AUTHOR.     MARCIO MARCOS.
      *
      *================================================================*
      * PROGRAMA BATCH MODELO COM DB2:
      * 1. DECLARA UM CURSOR PARA LER DADOS DO DB2
      * 2. FAZ O LOOP (FETCH) PELOS REGISTROS
      * 3. ATUALIZA O REGISTRO (UPDATE ... WHERE CURRENT OF CURSOR)
      * 4. FAZ 'COMMIT' PERIODICAMENTE
      * 5. GERA UM RELATORIO DE PROCESSAMENTO (RPT-FILE)
      *================================================================*
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      *--- ARQUIVO DE RELATORIO/LOG (EX: 133 BYTES P/ SYSOUT)
           SELECT RPT-FILE ASSIGN TO REPORTDD
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-RPT-FILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD RPT-FILE
          RECORD CONTAINS 133 CHARACTERS
          BLOCK CONTAINS 0 RECORDS
          RECORDING MODE IS F.
       01 RPT-RECORD                PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
      *----------------------------------------------------------------*
      * AREAS DE COMUNICACAO SQL E DCLGEN
      *----------------------------------------------------------------*
      *--- SQL COMMUNICATION AREA (SEMPRE NECESSARIA)
       01 SQLCA.
          EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      *--- DCLGEN (COPYBOOK) DA TABELA
       01 DCL-MY-TABLE.
          EXEC SQL INCLUDE MYDCLGEN END-EXEC.
      * (O DCLGEN contera as variaveis D-ID-CLIENTE, D-STATUS, etc.)
      *
      *----------------------------------------------------------------*
      * VARIAVEIS DE CONTROLE E FILE STATUS
      *----------------------------------------------------------------*
       01 WS-FILE-STATUS-FIELDS.
           05 WS-RPT-FILE-STATUS    PIC X(02).
              88 WS-RPT-OK          VALUE '00'.
      *
      *--- VARIAVEL DE CONTROLE DO SQLCODE (BOA PRATICA)
       01 WS-SQL-CONTROLE.
           05 WS-SQLCODE            PIC S9(09) COMP VALUE 0.
              88 SQL-OK             VALUE 0.
              88 SQL-NOT-FOUND      VALUE +100.
      *
      *----------------------------------------------------------------*
      * CONTADORES E FLAGS
      *----------------------------------------------------------------*
       01 WS-COUNTERS.
           05 WS-COUNT-ROWS-READ    PIC 9(09) VALUE ZERO.
           05 WS-COUNT-ROWS-UPDATED PIC 9(09) VALUE ZERO.
           05 WS-COMMIT-COUNTER     PIC 9(05) VALUE ZERO.
           05 WS-COMMIT-FREQUENCY   PIC 9(05) VALUE 1000. *> Commit a cada
      *
      *--- LINHAS DO RELATORIO
       01 WS-RPT-HEADER.
           05 RPT-CC                PIC X(01) VALUE '1'. *> Pula Pagina
           05 FILLER                PIC X(50) VALUE '*** RELATORIO'.
           05 FILLER                PIC X(13) VALUE ' DE '.
           05 FILLER                PIC X(20) VALUE 'PROCESSAMENTO DB2'.
           05 FILLER                PIC X(49) VALUE SPACES.
      *
       01 WS-RPT-SUMMARY-LINE.
           05 RPT-CC                PIC X(01) VALUE SPACES.
           05 RPT-MSG-DESC          PIC X(30).
           05 RPT-MSG-VALUE         PIC Z.ZZZ.ZZZ.ZZ9.
           05 FILLER                PIC X(94) VALUE SPACES.
      *
      *----------------------------------------------------------------*
      * DECLARACAO DO CURSOR (O CORACAO DO PROGRAMA)
      *----------------------------------------------------------------*
       01 WS-CURSOR-DECLARATION.
          EXEC SQL
              DECLARE C1 CURSOR FOR
                 SELECT ID_CLIENTE,
                        NOME_CLIENTE,
                        STATUS_REGISTRO
                 FROM MINHA_TABELA
                 WHERE STATUS_REGISTRO = 'PENDENTE'
                 FOR UPDATE OF STATUS_REGISTRO
          END-EXEC.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *
      *--- 1. INICIALIZACAO
           PERFORM 1000-INITIALIZE.
      *
      *--- 2. PROCESSAMENTO PRINCIPAL (LEITURA E LOGICA)
           PERFORM 2000-PROCESS-RECORDS UNTIL SQL-NOT-FOUND.
      *
      *--- 3. FINALIZACAO
           PERFORM 3000-FINALIZE.
      *
      *--- FIM
           DISPLAY 'PROGRAMA MYDB2BAT CONCLUIDO'.
           STOP RUN.
      *
      *================================================================*
      * PARAGRAFOS DE INICIALIZACAO
      *================================================================*
       1000-INITIALIZE.
           PERFORM 1100-OPEN-FILES.
      * GRAVA O CABECALHO DO RELATORIO
           MOVE WS-RPT-HEADER TO RPT-RECORD.
           PERFORM 4200-WRITE-REPORT.
      *
           INITIALIZE WS-COUNTERS.
      *
      * ABRE O CURSOR DB2
           EXEC SQL
               OPEN C1
           END-EXEC.
           PERFORM 9100-CHECK-SQLCODE. *> Verifica se OPEN falhou
      *
      * FAZ A PRIMEIRA LEITURA (PRIME FETCH)
           PERFORM 4100-FETCH-NEXT-ROW.
           EXIT.
      *
       1100-OPEN-FILES.
           OPEN OUTPUT RPT-FILE.
           IF NOT WS-RPT-OK
              DISPLAY 'ERRO FATAL NA ABERTURA DO RPT-FILE'
              DISPLAY 'FILE STATUS: ' WS-RPT-FILE-STATUS
              PERFORM 9910-FILE-ERROR-ABEND
           END-IF.
           EXIT.
      *
      *================================================================*
      * PARAGRAFOS DE PROCESSAMENTO
      *================================================================*
       2000-PROCESS-RECORDS.
      * A LOGICA DE NEGOCIO OCORRE AQUI
           PERFORM 2100-BUSINESS-LOGIC.
      *
      * VERIFICA SE E HORA DE FAZER COMMIT
           PERFORM 2200-CHECK-COMMIT.
      *
      * LE O PROXIMO REGISTRO
           PERFORM 4100-FETCH-NEXT-ROW.
           EXIT.
      *
       2100-BUSINESS-LOGIC.
           ADD 1 TO WS-COUNT-ROWS-READ.
      *
      * A LOGICA PRINCIPAL: ATUALIZA O STATUS NA TABELA
      * "WHERE CURRENT OF C1" ATUALIZA A LINHA ONDE O CURSOR ESTA
           EXEC SQL
              UPDATE MINHA_TABELA
                 SET STATUS_REGISTRO = 'PROCESSADO'
              WHERE CURRENT OF C1
           END-EXEC.
      *
           PERFORM 9100-CHECK-SQLCODE.
           ADD 1 TO WS-COUNT-ROWS-UPDATED.
           EXIT.
      *
       2200-CHECK-COMMIT.
           ADD 1 TO WS-COMMIT-COUNTER.
           IF WS-COMMIT-COUNTER GREATER THAN OR EQUAL
              WS-COMMIT-FREQUENCY
           THEN
              EXEC SQL COMMIT END-EXEC
              PERFORM 9100-CHECK-SQLCODE
              MOVE ZERO TO WS-COMMIT-COUNTER
              DISPLAY 'COMMIT REALIZADO. REGISTROS: '
                      WS-COUNT-ROWS-UPDATED
           END-IF.
           EXIT.
      *
      *================================================================*
      * PARAGRAFOS DE FINALIZACAO
      *================================================================*
       3000-FINALIZE.
      * FECHA O CURSOR
           EXEC SQL CLOSE C1 END-EXEC.
           PERFORM 9100-CHECK-SQLCODE.
      *
      * FAZ O COMMIT FINAL (PEGA O LOTE RESTANTE)
           EXEC SQL COMMIT END-EXEC.
           PERFORM 9100-CHECK-SQLCODE.
      *
           PERFORM 3100-WRITE-SUMMARY-REPORT.
           CLOSE RPT-FILE.
           EXIT.
      *
       3100-WRITE-SUMMARY-REPORT.
      * GRAVA TOTAIS NO ARQUIVO DE RELATORIO
           MOVE SPACES TO RPT-RECORD.
           MOVE 'TOTAL DE LINHAS LIDAS     : ' TO RPT-MSG-DESC.
           MOVE WS-COUNT-ROWS-READ TO RPT-MSG-VALUE.
           WRITE RPT-RECORD FROM WS-RPT-SUMMARY-LINE.

           MOVE SPACES TO RPT-RECORD.
           MOVE 'TOTAL DE LINHAS ATUALIZADAS: ' TO RPT-MSG-DESC.
           MOVE WS-COUNT-ROWS-UPDATED TO RPT-MSG-VALUE.
           WRITE RPT-RECORD FROM WS-RPT-SUMMARY-LINE.
           EXIT.
      *
      *================================================================*
      * PARAGRAFOS DE I/O E SQL (ROTINAS)
      *================================================================*
       4100-FETCH-NEXT-ROW.
      * BUSCA OS DADOS DO CURSOR PARA DENTRO DAS VARIAVEIS DO DCLGEN
           EXEC SQL
               FETCH C1
               INTO :D-ID-CLIENTE,
                    :D-NOME-CLIENTE,
                    :D-STATUS-REGISTRO
           END-EXEC.
      *
      * MOVE O SQLCODE PARA NOSSA VARIAVEL 88
           MOVE SQLCODE TO WS-SQLCODE.
      *
      * NAO ABENDA SE FOR 'NOT FOUND' (100), APENAS SE FOR ERRO (< 0)
           IF SQL-OK OR SQL-NOT-FOUND
              CONTINUE
           ELSE
              DISPLAY 'ERRO FATAL NO FETCH DO CURSOR C1'
              PERFORM 9900-SQL-ERROR-ABEND
           END-IF.
           EXIT.
      *
       4200-WRITE-REPORT.
           WRITE RPT-RECORD.
           IF NOT WS-RPT-OK
              DISPLAY 'ERRO FATAL DE ESCRITA NO RPT-FILE'
              DISPLAY 'FILE STATUS: ' WS-RPT-FILE-STATUS
              PERFORM 9910-FILE-ERROR-ABEND
           END-IF.
           EXIT.
      *
      *================================================================*
      * PARAGRAFOS DE ERRO/ABEND
      *================================================================*
       9100-CHECK-SQLCODE.
           MOVE SQLCODE TO WS-SQLCODE.
      *
      * ESTA ROTINA VERIFICA SE O SQL FOI OK (ZERO)
      * QUALQUER COISA DIFERENTE DISSO (INCLUINDO +100) E UM ERRO AQUI
           IF NOT SQL-OK
              PERFORM 9900-SQL-ERROR-ABEND
           END-IF.
           EXIT.
      *
       9900-SQL-ERROR-ABEND.
           DISPLAY '*** PROGRAMA MYDB2BAT ENCERRADO COM ERRO DE SQL ***'.
           DISPLAY 'SQLCODE : ' SQLCODE.
           DISPLAY 'SQLERRMC: ' SQLERRMC. *> Variavel da SQLCA
           DISPLAY '**************************************************'.
      *
      * DESFAZ QUALQUER ALTERACAO PENDENTE
           EXEC SQL ROLLBACK END-EXEC.
      *
           PERFORM 3000-FINALIZE. *> Tenta fechar arquivos
           MOVE 16 TO RETURN-CODE.
           STOP RUN.
      *
       9910-FILE-ERROR-ABEND.
           DISPLAY '*** PROGRAMA MYDB2BAT ENCERRADO COM ERRO DE ARQUIVO ***'.
           MOVE 16 TO RETURN-CODE.
           STOP RUN.