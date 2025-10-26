# Cobol
# 💻 Repositório de Modelos COBOL Mainframe (CICS, DB2 e Batch)

Este repositório contém exemplos de programas em COBOL que demonstram as principais arquiteturas e interações com sistemas mainframe, incluindo programas CICS (Online) e programas Batch (em lote) com acesso a arquivos VSAM e banco de dados DB2.

## 📁 Estrutura do Projeto

| Nome do Arquivo | Tipo de Programa | Tecnologia Principal |
| :--- | :--- | :--- |
| `MYCICS.cbl` | CICS Online (Básico) | Pseudo-Conversacional |
| `MYCICSBR.cbl` | CICS Online (Browse) | Acesso a Arquivo **VSAM KSDS** (`READNEXT`, `STARTBR`) |
| `MYCICSQL.cbl` | CICS Online (Consulta) | Acesso a **DB2** (`SELECT` simples) |
| `MYCICSBQ.cbl` | CICS Online (Browse/Paging) | Acesso a **DB2** com Múltiplos ** cursores** (`DECLARE/OPEN/FETCH`) |
| `MYBATCH.cbl` | Programa Batch (Modelo) | Processamento de Arquivos Sequenciais (Entrada, Saída, Relatório) |
| `MYDB2BAT.cbl` | Programa Batch (DB2) | Processamento e Atualização de **DB2** com **Cursor** (`FETCH`, `UPDATE WHERE CURRENT OF`) |

O que temos, por aqui....

## 🎯 Programas CICS (Online)

Os programas CICS utilizam a técnica **Pseudo-Conversacional** para manter o estado da sessão entre as interações do usuário.

### 1. MYCICS.cbl: Modelo Básico

* [cite_start]**Função:** Demonstra a estrutura fundamental de um programa CICS[cite: 209].
* **Lógica:**
    * [cite_start]Verifica o tamanho da `COMMAREA` (`EIBCALEN`) para saber se é a primeira execução ou um retorno do usuário[cite: 232, 233].
    * [cite_start]Na primeira vez, envia o mapa limpo (`MAPONLY`)[cite: 242].
    * [cite_start]No retorno, verifica a tecla pressionada (`EIBAID`)[cite: 247].
    * [cite_start]Se for **ENTER** (`DFHENTER`), recebe os dados do mapa (`EXEC CICS RECEIVE`)[cite: 250].
    * [cite_start]Se for **PF3** (`DFHPF3`), envia uma mensagem de encerramento e finaliza a tarefa (`EXEC CICS RETURN`)[cite: 259, 260].

### 2. MYCICSBR.cbl: Consulta com Paginação (VSAM)

* [cite_start]**Função:** Implementa a navegação (Browse) em um arquivo VSAM KSDS (`CUSTFILE`)[cite: 144].
* [cite_start]**Lógica:** Permite ao usuário avançar (**PF8**) e retroceder (**PF7**) páginas de dados[cite: 145, 173].
    * [cite_start]Utiliza os comandos CICS de I/O de arquivo: `STARTBR`, `READNEXT`, `READPREV` e `ENDBR`[cite: 185, 189, 199, 193].
    * [cite_start]A `COMMAREA` salva a primeira e a última chave de registro da tela (`CA-FIRST-KEY-SCREEN`, `CA-LAST-KEY-SCREEN`) para controle da paginação[cite: 157, 158].

### 3. MYCICSQL.cbl: Consulta Simples (DB2)

* [cite_start]**Função:** Realiza uma consulta simples (`SELECT`) no DB2, buscando dados de um cliente pelo ID[cite: 1].
* [cite_start]**Tecnologia:** Uso obrigatório do `INCLUDE SQLCA` e do `INCLUDE DCLGEN` (`DCLCLI`)[cite: 8, 9].
* [cite_start]**Lógica:** Aguarda o usuário digitar um ID e, ao pressionar ENTER, executa o `SELECT` e exibe o resultado ou a mensagem de erro (`SQLCODE`)[cite: 2, 3].

### 4. MYCICSBQ.cbl: Paginação Complexa (DB2)

* [cite_start]**Função:** Implementa a paginação (Browse) de dados do DB2 usando múltiplos cursores, mais eficiente para grandes volumes de dados[cite: 34].
* **Destaque:** Declaração de cursores específicos:
    * [cite_start]`C_FWD`: Para avançar na consulta[cite: 52].
    * [cite_start]`C_BACK`: Para retroceder, usando `ORDER BY DESC`[cite: 54, 55].
    * [cite_start]`C_START`: Para a carga inicial (`ID_CLIENTE >= :WS-START-KEY`)[cite: 56, 57].
* [cite_start]**Controle:** O estado da paginação (chaves e flags de fim/início de dados) é salvo na `COMMAREA`[cite: 46, 47, 48, 49].

---

## ⚙️ Programas Batch (Em Lote)

### 5. MYBATCH.cbl: Modelo Padrão Batch

* [cite_start]**Função:** Modelo de programa em lote para processamento de arquivos sequenciais[cite: 296].
* [cite_start]**Estrutura:** Segue a arquitetura padrão **Inicialização** (`1000-`), **Processamento** (`2000-`), e **Finalização** (`3000-`)[cite: 329, 330, 331].
* [cite_start]**I/O:** Define e processa arquivos de Entrada (`IN-FILE`), Saída (`OUT-FILE`) e Relatório/Log (`RPT-FILE`)[cite: 298, 299, 300].

### 6. MYDB2BAT.cbl: Processamento e Atualização (DB2)

* [cite_start]**Função:** Programa Batch que realiza um processamento e atualização massiva de dados no DB2[cite: 73].
* [cite_start]**Destaque:** Uso de cursor **atualizável** (`FOR UPDATE OF STATUS_REGISTRO`)[cite: 99].
* [cite_start]**Atualização:** Realiza a atualização do registro lido usando o comando `UPDATE ... WHERE CURRENT OF C1`[cite: 117, 118].
* [cite_start]**Transação:** Implementa a lógica de `COMMIT` periódico (a cada 1000 registros, definidos em `WS-COMMIT-FREQUENCY`) para evitar *lock* excessivo no DB2 e falhas de transação[cite: 87, 120].
* [cite_start]**Relatório:** Gera um relatório de resumo com o total de linhas lidas e atualizadas[cite: 127, 129].

--

GIT em atualizaÇÃo ....
