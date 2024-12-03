
# Analyse lexicale et syntaxique

# Analyse lexicale

Le code de l'analyseur lexical est donné dans le fichier `clexer.mll`. Il doit être compilé avec `ocamllex` pour obtenir le code OCaml qui reconnaît les lexemes définis pour ce langage.

Les catégories de lexemes définis sont :
- `digit` pour les constantes entières en base 10
- `letter` pour les caractètres alphabetiques
- `hex` pour les constantes en base 16,
- etc.

La definition qui permet d'obtenir l'analyseur est `ctoken`

Un test qui permet de valider les lexemes corrects est tst/ok_00.c
Un test qui montre qu'un lexeme incorrect (identificateur) est reconnu est tst/nok_01.c

# Analyse syntaxique

Le code de l'analyseur syntaxique est donné dans le fichier `cparser.mly`. Il doit être compilé avec `mehnir` pour obtenir le code OCaml qui reconnaît la grammaire du langage.

Le symbôle de start est `file`.

Un particularité de cette grammaire est qu'elle ne permet pas les définitions de fonctions imbriquées.
Aussi, les types adresse de fonction doivent être déclarés sous la forme `typeResultat (*) (liste type paramètres)`.

La grammaire ne permet pas les déclarations de fonctions (fonction sans corps).

Un test qui permet de valider un programme correct est tst/ok_02.c
Un test qui montre que la déclaration multiple de variables n'est pas permise est tst/nok_00.c



