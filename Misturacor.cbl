       IDENTIFICATION DIVISION.
       PROGRAM-ID. MISTURACOR.
       AUTHOR.     MARCIO MARCOS.
      *
      * ESTE PROGRAMA SIMULA A MISTURA DE CORES BASICAS.
      * ELE RECEBE DUAS CORES COMO ENTRADA E MOSTRA O RESULTADO.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * VARIAVEIS PARA ENTRADA DO USUARIO
      *----------------------------------------------------------------*
       01 WS-CORES-ENTRADA.
           05 WS-COR-1            PIC X(15).
           05 WS-COR-2            PIC X(15).
      *
      *----------------------------------------------------------------*
      * VARIAVEIS PARA PROCESSAMENTO (CONVERTIDAS P/ MAIUSCULAS)
      *----------------------------------------------------------------*
       01 WS-CORES-PROCESSADAS.
           05 WS-COR-1-UPPER      PIC X(15).
           05 WS-COR-2-UPPER      PIC X(15).
      *
      *----------------------------------------------------------------*
      * VARIAVEL DE SAIDA
      *----------------------------------------------------------------*
       01 WS-COR-RESULTADO         PIC X(25).

      *----------------------------------------------------------------*
      * VARIAVEL DE CONTROLE DE LOOP
      *----------------------------------------------------------------*
       01 WS-CONTROLE-LOOP         PIC X(1) VALUE 'S'.
           88 CONTINUAR-PROGRAMA             VALUE 'S', 's'.
           88 ENCERRAR-PROGRAMA              VALUE 'N', 'n'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *
      * CABECALHO DO PROGRAMA
      *
           DISPLAY "*************************************".
           DISPLAY "*** MISTURADOR DE CORES EM COBOL ***".
           DISPLAY "*************************************".
           DISPLAY " ".
           DISPLAY "Cores primarias: VERMELHO, AZUL, AMARELO".
           DISPLAY "Outras: BRANCO, PRETO".
           DISPLAY " ".
      *
      * INICIA O LOOP PRINCIPAL DO PROGRAMA
      *
           PERFORM UNTIL ENCERRAR-PROGRAMA
      *
      * LIMPA AS VARIAVEIS PARA A NOVA EXECUCAO
      *
              INITIALIZE WS-CORES-ENTRADA
                         WS-CORES-PROCESSADAS
                         WS-COR-RESULTADO
      *
      * OBTEM AS CORES DO USUARIO
      *
              PERFORM 100-OBTER-CORES
      *
      * PROCESSA A MISTURA
      *
              PERFORM 200-MISTURAR-CORES
      *
      * EXIBE O RESULTADO
      *
              PERFORM 300-EXIBIR-RESULTADO
      *
      * VERIFICA SE O USUARIO QUER CONTINUAR
      *
              DISPLAY "Deseja misturar outras cores? (S/N): "
			  DISPLAY " "
              ACCEPT WS-CONTROLE-LOOP
	  *
           END-PERFORM.
      *
      * FIM DO PROGRAMA
      *
           DISPLAY "Fim do programa."
           STOP RUN.
      *
      *----------------------------------------------------------------*
      * PARAGRAFO PARA OBTER A ENTRADA DO USUARIO
      *----------------------------------------------------------------*
       100-OBTER-CORES.
           DISPLAY "Digite a primeira cor: "
           ACCEPT WS-COR-1.

           DISPLAY "Digite a segunda cor: "
           ACCEPT WS-COR-2.
      *
      * CONVERTE A ENTRADA PARA MAIUSCULAS PARA FACILITAR A
      * LOGICA DE COMPARACAO.
      *
           MOVE FUNCTION UPPER-CASE(WS-COR-1) TO WS-COR-1-UPPER.
           MOVE FUNCTION UPPER-CASE(WS-COR-2) TO WS-COR-2-UPPER.
      *
      *----------------------------------------------------------------*
      * PARAGRAFO PRINCIPAL COM A LOGICA DE MISTURA
      *----------------------------------------------------------------*
       200-MISTURAR-CORES.
      *
      * REGRA 1: CORES IGUAIS
      *
           IF WS-COR-1-UPPER = WS-COR-2-UPPER
              MOVE WS-COR-1-UPPER TO WS-COR-RESULTADO
      *
      * REGRA 2: MISTURAS PRIMARIAS (A + B = B + A)
      *
           ELSE IF (WS-COR-1-UPPER = "AMARELO" AND WS-COR-2-UPPER = "AZUL")
                OR (WS-COR-1-UPPER = "AZUL"    AND WS-COR-2-UPPER = "AMARELO")
              MOVE "VERDE" TO WS-COR-RESULTADO

           ELSE IF (WS-COR-1-UPPER = "AMARELO" AND WS-COR-2-UPPER = "VERMELHO")
                OR (WS-COR-1-UPPER = "VERMELHO" AND WS-COR-2-UPPER = "AMARELO")
              MOVE "LARANJA" TO WS-COR-RESULTADO

           ELSE IF (WS-COR-1-UPPER = "AZUL" AND WS-COR-2-UPPER = "VERMELHO")
                OR (WS-COR-1-UPPER = "VERMELHO" AND WS-COR-2-UPPER = "AZUL")
              MOVE "ROXO" TO WS-COR-RESULTADO
      *
      * REGRA 3: MISTURANDO COM BRANCO (CLAREAR)
      *
           ELSE IF (WS-COR-1-UPPER = "AMARELO" AND WS-COR-2-UPPER = "BRANCO")
                OR (WS-COR-1-UPPER = "BRANCO" AND WS-COR-2-UPPER = "AMARELO")
              MOVE "AMARELO CLARO" TO WS-COR-RESULTADO

           ELSE IF (WS-COR-1-UPPER = "VERMELHO" AND WS-COR-2-UPPER = "BRANCO")
                OR (WS-COR-1-UPPER = "BRANCO" AND WS-COR-2-UPPER = "VERMELHO")
              MOVE "ROSA" TO WS-COR-RESULTADO

           ELSE IF (WS-COR-1-UPPER = "AZUL" AND WS-COR-2-UPPER = "BRANCO")
                OR (WS-COR-1-UPPER = "BRANCO" AND WS-COR-2-UPPER = "AZUL")
              MOVE "AZUL CLARO" TO WS-COR-RESULTADO

           ELSE IF (WS-COR-1-UPPER = "PRETO" AND WS-COR-2-UPPER = "BRANCO")
                OR (WS-COR-1-UPPER = "BRANCO" AND WS-COR-2-UPPER = "PRETO")
              MOVE "CINZA" TO WS-COR-RESULTADO
      *
      * REGRA 4: MISTURANDO COM PRETO (ESCURECER)
      *
           ELSE IF (WS-COR-1-UPPER = "VERMELHO" AND WS-COR-2-UPPER = "PRETO")
                OR (WS-COR-1-UPPER = "PRETO" AND WS-COR-2-UPPER = "VERMELHO")
              MOVE "VINHO (MARROM ESCURO)" TO WS-COR-RESULTADO

           ELSE IF (WS-COR-1-UPPER = "AZUL" AND WS-COR-2-UPPER = "PRETO")
                OR (WS-COR-1-UPPER = "PRETO" AND WS-COR-2-UPPER = "AZUL")
              MOVE "AZUL ESCURO (NOITE)" TO WS-COR-RESULTADO
      *
      * REGRA 5: COMBINACAO NAO PREVISTA
      *
           ELSE
              MOVE "COMBINACAO NAO PROGRAMADA" TO WS-COR-RESULTADO.
      *
      *----------------------------------------------------------------*
      * PARAGRAFO PARA MOSTRAR O RESULTADO FORMATADO
      *----------------------------------------------------------------*
       300-EXIBIR-RESULTADO.
           DISPLAY " ".
           DISPLAY "-----------------------------------------------".
           DISPLAY "RESULTADO DA MISTURA:".
           DISPLAY "   " WS-COR-1-UPPER " + " WS-COR-2-UPPER
           DISPLAY "   = " WS-COR-RESULTADO.
           DISPLAY "-----------------------------------------------".
           DISPLAY " ".