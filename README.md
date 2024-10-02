# Checkers

# Execução
Para iniciar o jogo execute o seguinte comando na raiz da aplicação:
```bash
swipl src/main.pl
```

Em seguida, dentro do interpretador do prolog, execute o comando:
```bash
start_game.
```

- Para mover uma peça, execute o comando: `mv(COORD1, COORD2).`
- Para capturar uma ou mais peças, execute o comando: `cap(COORD1, [COORD2, COORD3,..., COORDN]).`
- Para executar uma jogada feita pelo computador, execute o comando `computer_move().`
