       IDENTIFICATION DIVISION.
       PROGRAM-ID. MYCICSQL.
       AUTHOR.     MARCIO MARCOS.
      *
      *================================================================*
      * PROGRAMA CICS - CONSULTA SIMPLES DB2
      *
      * TRANSACAO: 'TRN3' (Exemplo)
      * MAPA:      'MYMAP3'
      * TABELA:    'TB_CLIENTES'
      *
      * LOGICA:
      * 1. Envia tela limpa.
      * 2. Usuario digita um ID e aperta ENTER.
      * 3. Programa le o ID, executa um 'SELECT' no DB2.
      * 4. Programa exibe os dados ou uma mensagem de erro na tela.
      *================================================================*
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *--- VARIAVEIS DE CONTROLE
       01 WS-PROGRAM-VARS.
           05 WS-MSG-SAIDA        PIC X(70).
           05 WS-ID-ENTRADA       PIC X(10).
      *
      *----------------------------------------------------------------*
      * AREAS DE COMUNICACAO SQL E DCLGEN
      *----------------------------------------------------------------*
      *--- SQL COMMUNICATION AREA (SEMPRE NECESSARIA)
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *
      *--- DCLGEN (COPYBOOK) DA TABELA TB_CLIENTES
           EXEC SQL INCLUDE DCLCLI END-EXEC.
      * (O DCLGEN contera as variaveis D-ID-CLIENTE, D-NOME-CLIENTE...)
      *
      *--- VARIAVEL DE CONTROLE DO SQLCODE (BOA PRATICA)
       01 WS-SQL-CONTROLE.
           05 WS-SQLCODE            PIC S9(09) COMP VALUE 0.
              88 SQL-OK             VALUE 0.
              88 SQL-NOT-FOUND      VALUE +100.
      *
      *--- MAPA SIMBOLICO (GERADO PELO BMS)
       COPY MYMAP3C.
      *
      *--- AREA DE COMUNICACAO (PARA "LEMBRAR" DADOS ENTRE EXECUCOES)
       01 WS-COMMAREA.
           05 CA-DUMMY             PIC X(01). *> Apenas para exemplo
      *
      *--- COPYBOOKS PADRAO CICS
       COPY DFHAID.
      *
       LINKAGE SECTION.
      *
       01 DFHEIBLK.
          COPY DFHEIBLK.
       01 DFHCOMMAREA             PIC X(01). *> Tamanho de WS-COMMAREA
      *
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       MAIN-PROCEDURE.
      *--- CONFIGURA O MANIPULADOR DE ERROS
           EXEC CICS HANDLE CONDITION
               ERROR(9000-CICS-ERROR)
               MAPFAIL(9100-MAPFAIL-ERROR)
           END-EXEC.
           EXEC CICS HANDLE ABEND
               PROGRAM(9900-PROGRAM-ABEND)
           END-EXEC.
      *
      *--- O CORACAO DO PROGRAMA: VERIFICA O TAMANHO DA COMMAREA
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
      * Logica para a primeira execucao da transacao.
           INITIALIZE WS-COMMAREA.
           INITIALIZE MYMAP3O. *> Limpa o mapa (Area de Output)
      *
           MOVE 'DIGITE O ID DO CLIENTE E TECLE ENTER:' TO MSGO.
      *
      *--- ENVIA O MAPA (TELA) PARA O TERMINAL
           EXEC CICS SEND
               MAP('MYMAP3')
               MAPSET('MYSET3')
               FROM(MYMAP3O)
               ERASE                 *> Apaga a tela
           END-EXEC.
      *
           PERFORM 4000-RETURN-TO-CICS.
           EXIT.
      *
       2000-SUBSEQUENT-TIME.
      * Logica para quando o usuario ja esta na tela e aperta algo.
           MOVE DFHCOMMAREA TO WS-COMMAREA.
      *
           EVALUATE EIBAID
      *
      * CASO 2A: Usuario apertou ENTER
               WHEN DFHENTER
                   PERFORM 2100-PROCESS-ENTER
      *
      * CASO 2B: Usuario apertou PF3 (Sair)
               WHEN DFHPF3
                   PERFORM 3000-PROCESS-EXIT
      *
      * CASO 2C: Outra tecla (Clear, PA1, etc.)
               WHEN OTHER
                   MOVE 'TECLA INVALIDA. USE ENTER OU PF3.' TO MSGO
                   PERFORM 4000-RETURN-TO-CICS
           END-EVALUATE.
           EXIT.
      *
       2100-PROCESS-ENTER.
      * Usuario apertou ENTER. Devemos ler os dados da tela.