# RSA em Common Lisp

Implementação educacional e robusta do algoritmo RSA em Common Lisp.
O projeto evoluiu para suportar **RSA‑2048**, **assinatura com SHA‑256** e **padding conforme a RFC 8017**, com fatiamento automático de mensagens longas.

## Funcionalidades Atuais

- **Suporte a RSA‑2048**: utiliza fatores primos longos para gerar um módulo seguro de 2048 bits.
- **Assinaturas com SHA‑256**: hash completo em `hash.lisp`.
- **Padding RFC 8017**: PKCS#1 v1.5 (tipo 02 para cifra, tipo 01 para assinatura).
- **Cálculo dinâmico de bloco**: detecta automaticamente o tamanho útil do bloco com base no módulo.
- **Fatiamento automático (chunking)**: mensagens longas são divididas em blocos conforme o tamanho da chave.

## Estrutura do Projeto

- **`math.lisp`**: núcleo aritmético (exponenciação modular e Euclides estendido).
- **`rsa.lisp`**: lógica central do RSA e cálculo de `key-octet-length`.
- **`primes.lisp`**: constantes dos primos usados (RSA‑100 e RSA‑2048).
- **`padding.lisp`**: implementação do padding PKCS#1 v1.5.
- **`hash.lisp`**: SHA‑256 completo, usado na assinatura digital.
- **`messenger.lisp`**: camada de aplicação (conversão texto/bytes, cifra, decifra, assinatura e verificação).
- **`main.lisp`**: script principal demonstrando cifra, decifra e verificação.
- **`test.lisp`**: teste simples com primos pequenos.

## Matemática Aplicada

O sistema baseia-se nas transformações fundamentais do RSA:

- **Cifragem**: $c = m^e \mod n$
- **Decifragem**: $m = c^d \mod n$
- **Assinatura**: $s = H(m)^d \mod n$
- **Verificação**: $H(m) == s^e \mod n$

## Como Rodar

Certifique-se de ter o [SBCL](http://www.sbcl.org/) instalado.
Para rodar a simulação completa (RSA‑2048 + SHA‑256):

```bash
sbcl --script main.lisp
```

## Como Gerar Novos Primos

Você pode gerar primos grandes usando o OpenSSL:

```bash
openssl prime -generate -bits 1024
```

### Exemplo de uso no `primes.lisp`

Após gerar dois primos com o OpenSSL, substitua os valores:

```lisp
(defconstant +rsa-2048-p+
  1234567890123456789012345678901234567890...)

(defconstant +rsa-2048-q+
  9876543210987654321098765432109876543210...)
```

## Exemplo de Saída

O programa gera chaves RSA‑2048, assina a mensagem com SHA‑256, cifra em blocos de 256 bytes e valida a assinatura após a decifragem.
