# RSA em Common Lisp

Uma implementação educacional, robusta e puramente funcional do algoritmo RSA em Common Lisp. O projeto evoluiu de um sistema simples para uma implementação capaz de processar chaves de 100 dígitos (RSA-100) com fatiamento automático de mensagens longas.

## Funcionalidades Atuais

- **Suporte a RSA-100**: Utiliza fatores primos de 50 dígitos para gerar um módulo $n$ de 100 dígitos decimais.
- **Primitivas RFC 8017**: Implementação fiel de `OS2IP` (Octet-String-to-Integer) e `I2OSP` (Integer-to-Octet-String).
- **Cálculo Dinâmico de Bloco**: O sistema detecta automaticamente o tamanho máximo de caracteres suportado pelo módulo $n$ (Key-Size Awareness).
- **Fatiamento Automático (Chunking)**: Mensagens que excedem a capacidade do módulo são automaticamente divididas em múltiplos blocos cifrados.

## Estrutura do Projeto

- **`math.lisp`**: Núcleo aritmético. Implementa Exponenciação Modular Binária e o Algoritmo de Euclides Estendido para inverso modular.
- **`rsa.lisp`**: Lógica central do RSA, geração de chaves (fixado em $e=65537$) e automação de `block-size`.
- **`primes.lisp`**: Repositório de constantes para chaves de larga escala (RSA-100).
- **`messenger.lisp`**: Camada de aplicação. Traduz texto para números gigantes e gerencia o fatiamento da mensagem.
- **`conversa.lisp`**: Script de simulação de alto nível simulando a troca de mensagens entre Ana e Beto.

## Matemática Aplicada

O sistema baseia-se nas transformações fundamentais do RSA:

- **Cifragem**: $c = m^e \pmod n$
- **Decifragem**: $m = c^d \pmod n$

Onde $m$ é a representação inteira da mensagem (ou bloco) obtida via `OS2IP`.

## Como Rodar

Certifique-se de ter o [SBCL](http://www.sbcl.org/) instalado. Para rodar a simulação completa:

```bash
sbcl --script conversa.lisp
```

## Exemplo de Saída

Ao processar uma mensagem longa, o sistema gera múltiplos blocos cifrados de 100 dígitos cada, garantindo que a carga útil nunca exceda o limite matemático do módulo.
