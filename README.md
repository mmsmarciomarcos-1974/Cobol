# 💻 Repositório de Modelos COBOL Mainframe (CICS, IMS, DB2 e Batch)

Este repositório é uma coleção de artefatos de desenvolvimento Mainframe criados para demonstrar o ciclo de vida completo de aplicações z/OS. O objetivo é apresentar exemplos práticos e funcionais de programas COBOL, tanto **Batch** (processamento em lote) quanto **Online** (**CICS** e **IMS DC**), interagindo com diferentes tipos de dados (**VSAM**, **DB2** e **IMS DB**).

Além dos códigos-fonte, o repositório inclui os **Mapas de Tela (BMS)** e os **JCLs (Job Control Language)** necessários para compilar, "linkar", "bindar", definir arquivos e executar essas aplicações, mostrando o processo completo desde o desenvolvimento até a implantação.

## 📁 Estrutura do Projeto

| Nome do Arquivo | Categoria | Tecnologia Principal | Propósito |
| :--- | :--- | :--- | :--- |
| **Programas COBOL** | | | |
| `MYCICS.cbl` | CICS Online | Pseudo-Conversacional | Demonstração CICS básica. |
| `MYCICSBR.cbl` | CICS Online | **VSAM KSDS** | Navegação (Browse) em arquivo VSAM. |
| `MYCICSQL.cbl` | CICS Online | **DB2** (SELECT) | Consulta simples em tabela DB2. |
| `MYCICSBQ.cbl` | CICS Online | **DB2** (Cursores) | Paginação complexa em tabela DB2. |
| `MYIMSONL.cbl` | IMS DC Online | MPP Básico | Demonstração IMS DC básica (`GU`/`ISRT`). |
| `MYIMSQL.cbl` | IMS DC Online | IMS DC / **DB2** | Consulta DB2 de um programa IMS. |
| `MYIMSBWS.cbl` | IMS DC Online | IMS DC / **IMS DB** | Paginação conversacional com **SPA** e `GN`. |
| `MYDB2BAT.cbl` | Batch | **DB2** (Cursor) | Atualização em lote com `UPDATE ... WHERE CURRENT OF`. |
| `Misturacor.cbl` | Batch | Lógica Pura | Demonstração de lógica Batch com `ACCEPT`/`DISPLAY`. |
| **Mapas de Tela** | | | |
| `MYMAP1C.bms.txt` | BMS | CICS Map | Mapa de tela para `MYCICS.cbl`. |
| `MYMAP2C.bms.txt` | BMS | CICS Map | Mapa de browse (10 linhas) para `MYCICSBR.cbl`. |
| `MYMAP3C.bms.txt` | BMS | CICS Map | Mapa de consulta para `MYCICSQL.cbl`. |
| `MYMAP4C.bms.txt` | BMS | CICS Map | Mapa de browse (10 linhas) para `MYCICSBQ.cbl`. |
| **JCLs** | | | |
| `JCLCOMP.jcl` | JCL | Compilação | Compila e Linka um programa COBOL Batch simples. |
| `ASMBMS.jcl` | JCL | Compilação | Assembla um Mapa BMS (gera Mapa Físico e Simbólico). |
| `COMPDB2.jcl` | JCL | Compilação | Compila, Linka e Binda um programa Batch/DB2. |
| `COMPCICS.jcl` | JCL | Compilação | Compila, Linka e Binda um programa CICS/DB2. |
| `RUNDB2.jcl` | JCL | Execução | Executa um programa Batch/DB2 via `IKJEFT01`. |
| `DEFVSAM.jcl` | JCL | Utilitário | Define (aloca) um arquivo VSAM KSDS via `IDCAMS`. |

---

## 🎯 Programas CICS (Online)

Os programas CICS utilizam a técnica **Pseudo-Conversacional** para manter o estado da sessão entre as interações do usuário.

### 1. MYCICS.cbl: Modelo Básico

* **Função:** Demonstra a estrutura fundamental de um programa CICS.
* **Lógica:**
    * Verifica o tamanho da `COMMAREA` (`EIBCALEN`) para saber se é a primeira execução ou um retorno do usuário.
    * Na primeira vez, envia o mapa limpo (`MAPONLY`).
    * No retorno, verifica a tecla pressionada (`EIBAID`).
    * Se for **ENTER** (`DFHENTER`), recebe os dados do mapa (`EXEC CICS RECEIVE`).
    * Se for **PF3** (`DFHPF3`), envia uma mensagem de encerramento e finaliza a tarefa (`EXEC CICS RETURN`).

### 2. MYCICSBR.cbl: Consulta com Paginação (VSAM)

* **Função:** Implementa a navegação (Browse) em um arquivo VSAM KSDS (`CUSTFILE`).
* **Lógica:** Permite ao usuário avançar (**PF8**) e retroceder (**PF7**) páginas de dados.
    * Utiliza os comandos CICS de I/O de arquivo: `STARTBR`, `READNEXT`, `READPREV` e `ENDBR`.
    * A `COMMAREA` salva a primeira e a última chave de registro da tela (`CA-FIRST-KEY-SCREEN`, `CA-LAST-KEY-SCREEN`) para controle da paginação.

### 3. MYCICSQL.cbl: Consulta Simples (DB2)

* **Função:** Realiza uma consulta simples (`SELECT`) no DB2, buscando dados de um cliente pelo ID.
* **Tecnologia:** Uso obrigatório do `INCLUDE SQLCA` e do `INCLUDE DCLGEN` (`DCLCLI`).
* **Lógica:** Aguarda o usuário digitar um ID e, ao pressionar ENTER, executa o `SELECT` e exibe o resultado ou a mensagem de erro (`SQLCODE`).

### 4. MYCICSBQ.cbl: Paginação Complexa (DB2)

* **Função:** Implementa a paginação (Browse) de dados do DB2 usando múltiplos cursores, mais eficiente para grandes volumes de dados.
* **Destaque:** Declaração de cursores específicos:
    * `C_FWD`: Para avançar na consulta.
    * `C_BACK`: Para retroceder, usando `ORDER BY DESC`.
    * `C_START`: Para a carga inicial (`ID_CLIENTE >= :WS-START-KEY`).
* **Controle:** O estado da paginação (chaves e flags de fim/início de dados) é salvo na `COMMAREA`.

---

## ⚙️ Programas Batch (Em Lote)

### 5. MYDB2BAT.cbl: Processamento e Atualização (DB2)

* **Função:** Programa Batch que realiza um processamento e atualização massiva de dados no DB2.
* **Destaque:** Uso de cursor **atualizável** (`FOR UPDATE OF STATUS_REGISTRO`).
* **Atualização:** Realiza a atualização do registro lido usando o comando `UPDATE ... WHERE CURRENT OF C1`.
* **Transação:** Implementa a lógica de `COMMIT` periódico (a cada 1000 registros, definidos em `WS-COMMIT-FREQUENCY`) para evitar *lock* excessivo no DB2 e falhas de transação.
* **Relatório:** Gera um relatório de resumo com o total de linhas lidas e atualizadas.

### 6. Misturacor.cbl: Lógica Batch Pura

* **Função:** Modelo de programa em lote simples que demonstra lógica de negócios, condicionais (`IF`/`EVALUATE`) e interação básica (`DISPLAY`/`ACCEPT`).

---

## 🛰️ Programas IMS DC (Online)

Os programas IMS DC (Data Communication) são a principal alternativa ao CICS para processamento online. Em vez de comandos `EXEC CICS`, eles usam chamadas `CALL 'CBLTDLI'` (Data Language/Interface) e interagem com **PCBs (Program Communication Blocks)** para se comunicar com o terminal e com bancos de dados.

### 7. MYIMSONL.cbl: Modelo Básico (MPP)

* **Função:** Demonstra a estrutura de um **MPP (Message Processing Program)**, o programa online IMS mais comum, em modo não-conversacional.
* **Lógica:**
    * Recebe o **IO-PCB** (o "ponteiro" para o terminal/usuário) na `ENTRY 'DLITCBL'`.
    * Usa `CALL 'CBLTDLI'` com a função **`GU`** (Get Unique) para ler a mensagem de entrada (o que o usuário digitou) a partir do `IO-PCB`.
    * Processa a lógica de negócio.
    * Usa `CALL 'CBLTDLI'` com a função **`ISRT`** (Insert) para enviar a mensagem de resposta de volta ao `IO-PCB` (terminal).

### 8. MYIMSQL.cbl: Consulta Simples (IMS/DB2)

* **Função:** Demonstra um programa híbrido muito comum: um programa online IMS DC que acessa um banco de dados **DB2**.
* **Lógica:**
    * Recebe a mensagem de entrada (com o ID do cliente) via **`GU`** no `IO-PCB`.
    * Usa o ID recebido para executar um `EXEC SQL SELECT ... END-EXEC` padrão no DB2 (esta parte é idêntica ao CICS).
    * Formata a resposta (seja o nome do cliente ou o `SQLCODE` de erro) em uma área de saída.
    * Envia a resposta de volta à tela via **`ISRT`** no `IO-PCB`.

### 9. MYIMSBWS.cbl: Paginação Conversacional (SPA)

* **Função:** Implementa a navegação (Browse) em um banco de dados (ex: IMS DB) usando uma **transação conversacional**.
* **Destaque:** Demonstra os equivalentes IMS para conceitos CICS:
    * **COMMAREA -> SPA (Scratch Pad Area):** A `ENTRY` do programa recebe o `SPA`, que é usado para salvar o estado da sessão (como a última chave lida) entre as interações do usuário.
    * **Arquivo (FCT/DD) -> DB-PCB:** O programa recebe um "PCB de Banco de Dados" (`DB-PCB`) que lhe dá acesso ao banco de dados hierárquico IMS DB.
    * **READNEXT -> 'GN' (Get Next):** O programa usa a chamada `GN` no `DB-PCB` para ler o próximo registro sequencialmente, permitindo a lógica de paginação.

    ⚙️ GIT em atualizaÇão, conforme disponibilidade
