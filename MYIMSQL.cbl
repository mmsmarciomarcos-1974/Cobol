	   IDENTIFICATION DIVISION.
       PROGRAM-ID. MYIMSQL.
       AUTHOR.     MARCIO MARCOS.
      *
      *================================================================*
      * PROGRAMA IMS/DC - CONSULTA SIMPLES DB2
      *
      * TRANSACAO: 'TRNIMS2' (Exemplo)
      *
      * LOGICA (EQUIVALENTE AO MYCICSQL.CBL):
      * 1. O USUARIO DIGITA A TRANSACAO + UM ID DE CLIENTE.
      * 2. O PROGRAMA LE A MENSAGEM DA TELA (GU no IO-PCB).
      * 3. O PROGRAMA USA O ID PARA FAZER UM 'SELECT' NO DB2.
      * 4. O PROGRAMA FORMATA A RESPOSTA (SUCESSO OU ERRO).
      * 5. O PROGRAMA ENVIA A RESPOSTA PARA A TELA (ISRT no IO-PCB).
      *================================================================*
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *--- VARIAVEIS DE CONTROLE DL/I
       01 WS-DLI-FUNCTIONS.
          05 WS-GU                 PIC X(04) VALUE 'GU  '.
          05 WS-ISRT               PIC X(04) VALUE 'ISRT'.
      *
      *--- AREAS DE MENSAGEM (MFS)
       01 WS-IN-MSG-AREA.
          05 WS-IN-LL              PIC S9(04) COMP.
          05 WS-IN-ZZ              PIC S9(04) COMP.
          05 WS-IN-TRANCODE        PIC X(08).
          05 WS-IN-ID-CLIENTE      PIC X(10).
          05 FILLER                PIC X(62).
      *
       01 WS-OUT-MSG-AREA.
          05 WS-OUT-LL             PIC S9(04) COMP VALUE 120.
          05 WS-OUT-ZZ             PIC S9(04) COMP VALUE 0.
          05 WS-OUT-MENSAGEM       PIC X(120).
      *
      *--- SQLCA E DCLGEN (IGUAIS AO CICS)
           EXEC SQL INCLUDE SQLCA END-EXEC.
           EXEC SQL INCLUDE DCLCLI END-EXEC.
      *
       LINKAGE SECTION.
      *
      *--- OBRIGATORIO: PCB DE I/O (TELA)
       01 IO-PCB-MASK.
          05 IO-LTERM-NAME         PIC X(08).
          05 FILLER                PIC X(02).
          05 IO-STATUS-CODE        PIC X(02).
          05 FILLER                PIC X(22).
      *
       PROCEDURE DIVISION.
       ENTRY 'DLITCBL' USING IO-PCB-MASK.
      *
       MAIN-PROCEDURE.
      *
      *--- 1. OBTER A MENSAGEM DE ENTRADA (COM O ID)
           CALL 'CBLTDLI' USING WS-GU
                                IO-PCB-MASK
                                WS-IN-MSG-AREA.
      *
      * (AQUI VERIFICARIA SE A LEITURA FOI OK, CHECANDO IO-STATUS-CODE)
      *
      *--- 2. EXECUTAR A CONSULTA DB2 (IDENTICO AO CICS)
           MOVE WS-IN-ID-CLIENTE TO D-ID-CLIENTE.
      *
           EXEC SQL
               SELECT NOME_CLIENTE
               INTO :D-NOME-CLIENTE
               FROM TB_CLIENTES
               WHERE ID_CLIENTE = :D-ID-CLIENTE
           END-EXEC.
      *
      *--- 3. TRATAR O RETORNO DO SQL
           EVALUATE SQLCODE
               WHEN 0
                   STRING "CLIENTE: " D-NOME-CLIENTE
                       DELIMITED BY SIZE
                       INTO WS-OUT-MENSAGEM
               WHEN 100
                   MOVE "CLIENTE NAO ENCONTRADO."
                       TO WS-OUT-MENSAGEM
               WHEN OTHER
                   MOVE "ERRO GRAVE NO BANCO DE DADOS."
                       TO WS-OUT-MENSAGEM
           END-EVALUATE.
      *
      *--- 4. ENVIAR A RESPOSTA PARA A TELA
           CALL 'CBLTDLI' USING WS-ISRT
                                IO-PCB-MASK
                                WS-OUT-MSG-AREA.
      *
           GOBACK.