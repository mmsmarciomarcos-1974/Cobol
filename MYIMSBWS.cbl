	   IDENTIFICATION DIVISION.
       PROGRAM-ID. MYIMSBWS.
       AUTHOR.     MARCIO MARCOS.
      *
      *================================================================*
      * PROGRAMA IMS/DC - BROWSE CONVERSACIONAL (COM SPA)
      *
      * TRANSACAO: 'TRNIMSB' (Exemplo, definida como CONVERSATIONAL)
      * BANCO:     'DBCLIENT' (Banco de dados IMS DB)
      *
      * LOGICA (EQUIVALENTE AO MYCICSBR.CBL):
      * 1. O IMS PASSA O 'SPA' (SCRATCH PAD AREA) E A MENSAGEM.
      * 2. SE O SPA ESTIVER EM BRANCO (1A VEZ):
      * - BUSCA A PARTIR DO INICIO (CHAVE LOW-VALUES).
      * 3. SE O SPA TIVER UMA CHAVE (PF8 - AVANCAR):
      * - BUSCA A PARTIR DA ULTIMA CHAVE SALVA + 1.
      * 4. O PROGRAMA USA 'GN' (GET NEXT) PARA LER 10 REGISTROS.
      * 5. O PROGRAMA SALVA A CHAVE DO ULTIMO REGISTRO LIDO NO SPA.
      * 6. O PROGRAMA ENVIA A TELA (ISRT) E O SPA ATUALIZADO.
      *================================================================*
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *--- VARIAVEIS DE CONTROLE DL/I
       01 WS-DLI-FUNCTIONS.
          05 WS-GU                 PIC X(04) VALUE 'GU  '.
          05 WS-ISRT               PIC X(04) VALUE 'ISRT'.
          05 WS-GN                 PIC X(04) VALUE 'GN  '. *> Get Next
      *
      *--- AREA DA MENSAGEM DE ENTRADA (TELA)
       01 WS-IN-MSG-AREA.
          05 WS-IN-LL              PIC S9(04) COMP.
          05 WS-IN-ZZ              PIC S9(04) COMP.
          05 WS-IN-TRANCODE        PIC X(08).
          05 WS-IN-PFKEY           PIC X(01). *> Ex: '8' p/ PF8
      *
      *--- AREA DA MENSAGEM DE SAIDA (TELA)
       01 WS-OUT-MSG-AREA.
          05 WS-OUT-LL             PIC S9(04) COMP.
          05 WS-OUT-ZZ             PIC S9(04) COMP.
          05 WS-OUT-LINHAS OCCURS 10 TIMES.
             10 WS-OUT-ID          PIC X(10).
             10 WS-OUT-NOME        PIC X(40).
          05 WS-OUT-MSG-ERRO       PIC X(70).
      *
      *--- SEGMENTO DO BANCO DE DADOS IMS (COMO UM DCLGEN/FD)
       01 SEG-CLIENTE.
          05 SEG-CLI-ID            PIC X(10).
          05 SEG-CLI-NOME          PIC X(40).
          05 FILLER                PIC X(50).
      *
      *--- SSA: SEGMENT SEARCH ARGUMENT (FILTRO DE PESQUISA)
       01 WS-SSA-CLIENTE.
          05 SSA-SEGNAME           PIC X(08) VALUE 'CLIENTE '.
          05 SSA-LPAREN            PIC X(01) VALUE '('.
          05 SSA-KEYNAME           PIC X(08) VALUE 'CLIID   '.
          05 SSA-OPERATOR          PIC X(02) VALUE '>=_'. *> _ = espaco
          05 SSA-KEY-VALUE         PIC X(10).
          05 SSA-RPAREN            PIC X(01) VALUE ')'.
      *
       LINKAGE SECTION.
      *
      *--- PCB DE I/O (TELA)
       01 IO-PCB-MASK.
          05 IO-LTERM-NAME         PIC X(08).
          05 FILLER                PIC X(02).
          05 IO-STATUS-CODE        PIC X(02).
          05 FILLER                PIC X(22).
      *
      *--- PCB DO BANCO DE DADOS IMS (EQUIVALENTE AO 'FILE' VSAM)
       01 DB-PCB-MASK.
          05 DB-DBD-NAME           PIC X(08).
          05 DB-SEG-LEVEL          PIC X(02).
          05 DB-STATUS-CODE        PIC X(02).
          05 FILLER                PIC X(12).
          05 DB-KEY-FEEDBACK       PIC X(10). *> Chave lida
      *
      *--- SPA: SCRATCH PAD AREA (EQUIVALENTE A COMMAREA)
       01 SPA-AREA.
          05 SPA-LL                PIC S9(04) COMP.
          05 SPA-ZZ                PIC S9(04) COMP.
          05 SPA-LAST-KEY-SCREEN   PIC X(10).
      *
       PROCEDURE DIVISION.
       ENTRY 'DLITCBL' USING IO-PCB-MASK
                             DB-PCB-MASK
                             SPA-AREA.
      *
       MAIN-PROCEDURE.
      *
      *--- 1. OBTER A MENSAGEM DE ENTRADA (QUE VEM COM O SPA)
      * (O 'GU' NO IO-PCB TAMBEM TRAZ O SPA SE A TRAN FOR CONV.)
           CALL 'CBLTDLI' USING WS-GU
                                IO-PCB-MASK
                                WS-IN-MSG-AREA
                                SPA-AREA.
      *
           PERFORM 2000-PROCESSAR-PAGINACAO.
      *
      *--- 3. ENVIAR RESPOSTA E SALVAR O SPA
      * (O 'ISRT' NO IO-PCB SALVA O SPA JUNTO COM A MSG)
           CALL 'CBLTDLI' USING WS-ISRT
                                IO-PCB-MASK
                                WS-OUT-MSG-AREA
                                SPA-AREA.
      *
           GOBACK.
      *
       2000-PROCESSAR-PAGINACAO.
           INITIALIZE WS-OUT-MSG-AREA.
      *
      *--- 2. DEFINIR A CHAVE DE INICIO
           IF SPA-LAST-KEY-SCREEN = SPACES OR LOW-VALUES
              MOVE LOW-VALUES TO SSA-KEY-VALUE
              MOVE '>=' TO SSA-OPERATOR
           ELSE
              MOVE SPA-LAST-KEY-SCREEN TO SSA-KEY-VALUE
              MOVE '> ' TO SSA-OPERATOR
           END-IF.
      *
      *--- 3. LOOP DE LEITURA (EQUIV. AO READNEXT)
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
      *
      * CHAMA 'GN' (GET NEXT) NO 'DB-PCB'
              CALL 'CBLTDLI' USING WS-GN
                                   DB-PCB-MASK
                                   SEG-CLIENTE
                                   WS-SSA-CLIENTE
      *
      * VERIFICA O STATUS DA LEITURA
              IF DB-STATUS-CODE = 'GE' *> 'Get End' (Not Found)
                 MOVE 'FIM DOS DADOS' TO WS-OUT-MSG-ERRO
                 EXIT PERFORM
              END-IF
      *
              IF DB-STATUS-CODE NOT = '  '
                 MOVE 'ERRO NA LEITURA DO IMS DB' TO WS-OUT-MSG-ERRO
                 EXIT PERFORM
              END-IF
      *
      *--- PREENCHE A LINHA E SALVA A ULTIMA CHAVE
              MOVE SEG-CLI-ID TO WS-OUT-ID(I)
              MOVE SEG-CLI-NOME TO WS-OUT-NOME(I)
      *
      * SALVA A CHAVE NO SPA PARA A PROXIMA INTERACAO
              MOVE SEG-CLI-ID TO SPA-LAST-KEY-SCREEN
      *
           END-PERFORM.