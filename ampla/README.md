# AmplaApp - Estrutura Modular

## Visão Geral

A aplicação foi reestruturada seguindo uma arquitetura modular para melhorar a organização, manutenibilidade e reutilização do código.

## Convenção de Nomenclatura

### Módulos Principais (`m_`)
Arquivos que começam com `m_` são módulos principais da aplicação. Cada módulo representa uma funcionalidade completa ou seção da aplicação.

**Módulos Existentes:**
- `m_inicio.R` - Página inicial com navegação
- `m_despesas.R` - Módulo de análise de despesas
- `m_receitas.R` - Módulo de análise de receitas

### Submódulos (`sm_`)
Arquivos que começam com `sm_` são submódulos/componentes reutilizáveis que podem ser utilizados em diferentes módulos principais.

**Submódulos Existentes:**
- `sm_filtro_periodo.R` - Componente de filtro de período (usado em análises financeiras)
- `sm_grafico_barras_empilhadas.R` - Componente de gráficos de barras empilhadas (usado em análises)

## Estrutura de Arquivos

```
ampla/R/
├── app_ui.R                          # Interface principal da aplicação
├── app_server.R                      # Servidor principal da aplicação
├── run_app.R                         # Função para executar a aplicação
├── m_inicio.R                        # Módulo: Página inicial
├── m_despesas.R                      # Módulo: Análise de despesas
├── m_receitas.R                      # Módulo: Análise de receitas
├── sm_filtro_periodo.R               # Submódulo: Filtro de período
└── sm_grafico_barras_empilhadas.R    # Submódulo: Gráficos de barras
```

## Como Funciona

### 1. Interface Principal (`app_ui.R`)
- Detecta parâmetros da URL (?page=despesas, ?page=receitas)
- Chama o módulo apropriado baseado na página solicitada
- Implementa roteamento simples

### 2. Servidor Principal (`app_server.R`)
- Carrega os dados necessários
- Inicializa todos os módulos principais
- Passa dados para os módulos conforme necessário

### 3. Módulos Principais
Cada módulo principal (`m_*`) contém:
- Função UI: `m_*_ui(id)`
- Função Server: `m_*_server(id, ...)`
- Lógica específica do módulo
- Uso de submódulos quando apropriado

### 4. Submódulos
Cada submódulo (`sm_*`) contém:
- Função UI: `sm_*_ui(id)`
- Função Server: `sm_*_server(id, ...)`
- Funcionalidade reutilizável
- Pode ser usado em múltiplos módulos principais

## Vantagens da Estrutura Modular

1. **Organização**: Código bem estruturado e fácil de navegar
2. **Manutenibilidade**: Mudanças isoladas em módulos específicos
3. **Reutilização**: Submódulos podem ser usados em diferentes contextos
4. **Escalabilidade**: Fácil adição de novos módulos e funcionalidades
5. **Testabilidade**: Módulos podem ser testados independentemente

## Adicionando Novos Módulos

### Para adicionar um novo módulo principal:

1. Crie um arquivo `m_nome_modulo.R`
2. Implemente as funções `m_nome_modulo_ui()` e `m_nome_modulo_server()`
3. Adicione a chamada em `app_ui.R` e `app_server.R`
4. Atualize o NAMESPACE se necessário

### Para adicionar um novo submódulo:

1. Crie um arquivo `sm_nome_submodulo.R`
2. Implemente as funções `sm_nome_submodulo_ui()` e `sm_nome_submodulo_server()`
3. Use o submódulo nos módulos principais que precisarem
4. Atualize o NAMESPACE se necessário

## Uso

A aplicação mantém a mesma interface externa:

```r
# Executar aplicação
run_app()

# Acessar páginas específicas
# http://localhost:3838/?page=despesas
# http://localhost:3838/?page=receitas
# http://localhost:3838/ (dashboard principal)
```
