       IDENTIFICATION DIVISION.
       PROGRAM-ID. MYBATCH.
       AUTHOR.     MARCIO MARCOS.
       DATE-WRITTEN. 26/10/2025.
      *
      *================================================================*
      * PROGRAMA BATCH MODELO:
      * 1. LE UM ARQUIVO DE ENTRADA (IN-FILE)
      * 2. PROCESSA OS DADOS (LOGICA DE NEGOCIO)
      * 3. GERA UM ARQUIVO DE SAIDA (OUT-FILE)
      * 4. GERA UM RELATORIO DE PROCESSAMENTO (RPT-FILE)
      *================================================================*
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      *--- ARQUIVO DE ENTRADA (EX: 80 BYTES, SEQUENCIAL)
           SELECT IN-FILE ASSIGN TO INPUTDD
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-IN-FILE-STATUS.
      *
      *--- ARQUIVO DE SAIDA DE DADOS (EX: 100 BYTES)
           SELECT OUT-FILE ASSIGN TO OUTPUTDD
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE IS SEQUENTIAL
                  FILE STATUS IS WS-OUT-FILE-STATUS.
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
       FD IN-FILE
          RECORD CONTAINS 80 CHARACTERS
          BLOCK CONTAINS 0 RECORDS
          RECORDING MODE IS F.
       01 IN-RECORD                 PIC X(80).
      *
       FD OUT-FILE
          RECORD CONTAINS 100 CHARACTERS
          BLOCK CONTAINS 0 RECORDS
          RECORDING MODE IS F.
       01 OUT-RECORD                PIC X(100).
      *
       FD RPT-FILE
          RECORD CONTAINS 133 CHARACTERS
          BLOCK CONTAINS 0 RECORDS
          RECORDING MODE IS F.
       01 RPT-RECORD                PIC X(133).
      *
       WORKING-STORAGE SECTION.
      *
      *--- FILE STATUS
       01 WS-FILE-STATUS-FIELDS.
           05 WS-IN-FILE-STATUS     PIC X(02).
              88 WS-IN-OK           VALUE '00'.
              88 WS-IN-EOF          VALUE '10'.
           05 WS-OUT-FILE-STATUS    PIC X(02).
              88 WS-OUT-OK          VALUE '00'.
           05 WS-RPT-FILE-STATUS    PIC X(02).
              88 WS-RPT-OK          VALUE '00'.
      *
      *--- CONTADORES E TOTAIS
       01 WS-COUNTERS.
           05 WS-COUNT-IN-READ      PIC 9(07) VALUE ZERO.
           05 WS-COUNT-OUT-WRITTEN  PIC 9(07) VALUE ZERO.
           05 WS-COUNT-IN-ERROR     PIC 9(07) VALUE ZERO.
      *
      *--- LAYOUTS DE REGISTROS DE TRABALHO
       01 WS-IN-RECORD-LAYOUT.
           05 WS-IN-ID              PIC X(10).
           05 WS-IN-DATA            PIC X(70).
      *
       01 WS-OUT-RECORD-LAYOUT.
           05 WS-OUT-ID             PIC X(10).
           05 WS-OUT-TIMESTAMP      PIC X(26).
           05 WS-OUT-DATA           PIC X(64).
      *
      *--- LINHAS DO RELATORIO
       01 WS-RPT-HEADER.
           05 RPT-CC                PIC X(01) VALUE '1'. *> Pula Pagina
           05 FILLER                PIC X(50) VALUE '*** RELATORIO'.
           05 FILLER                PIC X(13) VALUE ' DE '.
           05 FILLER                PIC X(20) VALUE 'PROCESSAMENTO BATCH'.
           05 FILLER                PIC X(49) VALUE SPACES.
      *
       01 WS-RPT-SUMMARY-LINE.
           05 RPT-CC                PIC X(01) VALUE SPACES.
           05 RPT-MSG-DESC          PIC X(30).
           05 RPT-MSG-VALUE         PIC Z.ZZZ.ZZ9.
           05 FILLER                PIC X(94) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *
      *--- 1. INICIALIZACAO
           PERFORM 1000-INITIALIZE.
      *
      *--- 2. PROCESSAMENTO PRINCIPAL (LEITURA E LOGICA)
           PERFORM 2000-PROCESS-RECORDS UNTIL WS-IN-EOF.
      *
      *--- 3. FINALIZACAO
           PERFORM 3000-FINALIZE.
      *
      *--- FIM
           DISPLAY 'PROGRAMA MYBATCH CONCLUIDO'.
           STOP RUN.
      *
      *================================================================*
      * PARAGRAFOS DE INICIAL