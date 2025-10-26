	   IDENTIFICATION DIVISION.
       PROGRAM-ID. MYIMSONL.
       AUTHOR.     MARCIO MARCOS.
      *
      *================================================================*
      * PROGRAMA IMS/DC - MODELO BASICO (NAO-CONVERSACIONAL)
      *
      * TRANSACAO: 'TRNIMS1' (Exemplo)
      *
      * LOGICA (EQUIVALENTE AO MYCICS.CBL):
      * 1. O USUARIO DIGITA A TRANSACAO (E TALVEZ UM DADO).
      * 2. O IMS COLOCA A MENSAGEM NA FILA.
      * 3. O PROGRAMA E CARREGADO, LE A MENSAGEM (GU).
      * 4. O PROGRAMA ENVIA UMA RESPOSTA (ISRT).
      * 5. O PROGRAMA TERMINA.
      *
      * NOTA: O MAPA DE TELA E DEFINIDO FORA DO COBOL, NO MFS
      * (MESSAGE FORMAT SERVICE), O EQUIVALENTE IMS DO BMS.
      *================================================================*
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *--- VARIAVEIS DE CONTROLE DL/I (DATABASE/DATA COMMS)
       01 WS-DLI-FUNCTIONS.
          05 WS-GU                 PIC X(04) VALUE 'GU  '. *> Get Unique
          05 WS-ISRT               PIC X(04) VALUE 'ISRT'. *> Insert
      *
      *--- AREAS DE MENSAGEM (MAPEADAS PELO MFS)
       01 WS-IN-MSG-AREA.
          05 WS-IN-LL              PIC S9(04) COMP VALUE 100.
          05 WS-IN-ZZ              PIC S9(04) COMP VALUE 0.
          05 WS-IN-TRANCODE        PIC X(08).
          05 WS-IN-DADO            PIC X(72). *> Resto da msg
      *
       01 WS-OUT-MSG-AREA.
          05 WS-OUT-LL             PIC S9(04) COMP VALUE 80.
          05 WS-OUT-ZZ             PIC S9(04) COMP VALUE 0.
          05 WS-OUT-DADO           PIC X(80).
      *
       LINKAGE SECTION.
      *
      *----------------------------------------------------------------*
      * PCB (PROGRAM COMMUNICATION BLOCK)
      *
      * O 'I/O PCB' E O EQUIVALENTE DO TERMINAL CICS. E POR ONDE
      * AS MENSAGENS DE ENTRADA (TELA) CHEGAM E AS DE SAIDA SAEM.
      *----------------------------------------------------------------*
       01 IO-PCB-MASK.
          05 IO-LTERM-NAME         PIC X(08).
          05 FILLER                PIC X(02).
          05 IO-STATUS-CODE        PIC X(02).
          05 FILLER                PIC X(22). *> Restante da mascara
      *
       PROCEDURE DIVISION.
      *
      *----------------------------------------------------------------*
      * OBRIGATORIO: O PONTO DE ENTRADA DO PROGRAMA IMS.
      * O IMS PASSA OS ENDERECOS DOS 'PCBS' (COMO O IO-PCB)
      * QUE FORAM DEFINIDOS NO PSBGEN DESTE PROGRAMA.
      *----------------------------------------------------------------*
       ENTRY 'DLITCBL' USING IO-PCB-MASK.
      *
       MAIN-PROCEDURE.
      *
      *--- 1. OBTER A MENSAGEM DE ENTRADA (EQUIV. AO RECEIVE MAP)
      * CHAMA A INTERFACE DL/I, USANDO A FUNCAO 'GU' (GET UNIQUE)
      * NO 'IO-PCB' (A TELA), E COLOCA EM 'WS-IN-MSG-AREA'.
           CALL 'CBLTDLI' USING WS-GU
                                IO-PCB-MASK
                                WS-IN-MSG-AREA.
      *
      * (AQUI O PROGRAMA PODERIA FAZER UMA LOGICA DE NEGOCIO)
      *
           MOVE 'BEM-VINDO AO IMS DC!' TO WS-OUT-DADO.
      *
      *--- 2. ENVIAR A MENSAGEM DE RESPOSTA (EQUIV. AO SEND MAP)
      * CHAMA A INTERFACE DL/I, USANDO 'ISRT' (INSERT)
      * NO 'IO-PCB', ENVIANDO O CONTEUDO DE 'WS-OUT-MSG-AREA'.
           CALL 'CBLTDLI' USING WS-ISRT
                                IO-PCB-MASK
                                WS-OUT-MSG-AREA.
      *
      *--- 3. FIM
           GOBACK.