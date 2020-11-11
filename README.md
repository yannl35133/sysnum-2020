# Simulateur de netlist, Yann Leray

## Compiler et exécuter

- Compilation: via dune (le simulateur est écrit en OCaml) : `dune build`

- Exécution: via dune encore une fois (cette commande ne nécessite pas la précédente) `dune exec src/netlist.exe [OPTIONS] TEST_FILE`  
Les tests sont dans le dossier `test`

- Accéder aux exécutables : Ajouter la ligne `(promote (until-clean))` au fichier `dune`, dans le champ `executables`, puis rebuild.

### Arguments de l'exécutable

Le simulateur accepte les options :
-   `--print-only` (explicite),
-   `--debug` (dump les valeurs des variables et les états des mémoires à chaque étape),
-   `-n NUMBER` (lance la simulation pendant `NUMBER` cycles, défaut: `max_int` de OCaml),
-   `-i` (initialise les mémoires et registres à 1),
-   `-u` (initialise les mémoires et registres dans un état non stabilisé, qui se propage tant qu'il peut)

## Features non-standard

Comme précisé avec le flag `-u`, les fils ont trois états : On, Off et Unstabilized. Le troisième état est similaire à un état aléatoire, il n'est pas stable et donc inutilisable. Il se propage aussi loin qu'il peut, jusqu'à ce qu'il soit écrasé par des signaux stables. Il se propage extrêmement vite s'il est utilisé comme adresse d'écriture, car une nappe de fils interprétée comme entier, si elle contient un fil non stabilisé, sera interprétée comme instable sur tout le domaine de la nappe et, comme adresse d'écriture, corrompra toute la RAM (écriture considérée comme partielle).

Cela permet de s'assurer que toutes les initialisations sont effectuées correctement.

De plus, les opérations booléennes sont parallélisées. Ceci retire la nécessité de séparer les types Bit et BitArray de la spécification.

Enfin, un typeur a été ajouté. Il vérifie la bonne déclaration des variables, entrées et sorties, ainsi que les types (soit tailles) des différentes valeurs

## Vitesse de simulation

Le simulateur tourne environ deux fois moins vite que la version compilée fournie en entrée. En effet, là n'était pas le but. C'est un simulateur qui fait des vérifications initiales et n'est pas du tout optimisé pour la vitesse. Je le transformerai peut-être en compilateur vers C, mais je dois d'abord fixer la syntaxe des netlists que je me laisse le droit de modifier.

## Git

Le simulateur a git pour un contrôleur de version. Il est hébergé sur [Github](https://github.com/yannl35133/sysnum-2020/tree/job-done). L'historique est probablement assez malpropre et peu agréable pour naviguer entre les versions.