# RSA em Common Lisp

Uma implementação educacional e puramente funcional do algoritmo RSA, focada em recursão e clareza lógica.

## Estrutura do Projeto
 - **math.lisp**: Núcleo matemático (Exponenciação Modular e Euclides Estendido).
 - **rsa.lisp**: Lógica de geração de chaves e transformações RSA.
 - **messenger.lisp**: Camada de tradução de texto (String para Inteiros).
 - **conversa.lisp**: Simulação de troca de mensagens entre Ana e Beto.
 - **main.lisp**: Testes unitários básicos.

## Matemática Utilizada
 - Criptografia: $c = m^e \pmod n$
 - Descriptografia: $m = c^d \pmod n$

## Como Rodar
Você pode rodar a simulação completa usando SBCL:
```bash
sbcl --script conversa.lisp
```
