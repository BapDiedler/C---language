
# Typage de C--

Le code de l'analyseur de type est dans le fichier `ctyping.ml`.

Il est composé de quatres parties:

## Exceptions area

Les exceptions définis sont :
- `Error` avec comme arguments une localisation et un string.
- `Anomaly` avec comme argument un string. (Ne doit pas se produire)

## Variable area

Deux `Hashtbl` sont déclarées.
`env` contiendra :
- les fonctions qui seront déclarées dans le code
- les variables globales de celui-ci
- les variables accessibles en fonction du bloc
À la fin seules les fonctions et les variables globales seront dans la hash-table.
`env_fun_global` contiendra des couples nom de fonction et un entier:
- la valeur vaut `1` si la fonction est redéfinie localement
- la valeur vaut `0` si la fonction n'est pas redéfinit

## Functions area

On y retrouve des fonctions intermédiaires comme :
- `string_of_typ` qui permet d'avoir un string correspondant un type
- `equal_typ_var` qui teste si deux types sont égaux
- `typ_of_ctyp` qui permet de passer d'un ctyp vers un typ
- `check_fun_args` vérifie le type des arguments d'une fonction
- etc

## Mains functions area

Dans cette partie, le code vérifie :
- `epression` comme `operation`, `call`, `set_val` , etc
- `var_declaration` comme `cfun` ou `cdecl`
- `code`
- `block` en regardant la déclaration des variables et ses différents codes

C'est d'ailleur ici que l'on retrouve `check_file` qui est la fonction principale de ce ficher.