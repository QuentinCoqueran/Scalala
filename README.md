# Projet AL

## Pré-requis

Il est indispensable d'avoir installé en local:

- la version 2.13 du compilateur Scala, [ici](https://scala-lang.org/download/)

- le gestionnaire de build `sbt`, [voir ici](https://www.scala-sbt.org/download.html). En installant `sbt`, le compilateur sera installé aussi.

## Structure du projet

Ceci est le projet de validation du cours d'initiation à la programmation fonctionnelle. Le code source doit être écrit dans le répertoire `./src/main/scala`. Vous pourrez créer autant de package que vous voulez.
Les tests unitaires doivent être écrit dans le répertoire `./src/test/scala`. Pour écrire des tests unitaires, veuillez vous reporter à la section [Tests Unitaires](#tests-unitaires).

## Guide de survie avec sbt

Ce projet est un application Scala standalone. Il est géré par `sbt`, le build tool Scala. Sa documentation est disponible [ici](https://www.scala-sbt.org/1.x/docs/).

Nous allons lister ici une liste de commandes utiles avec `sbt`:

- `sbt`: cette commande lance un invite de commande interactif

- `run` (ou `sbt run` hors de l'invite de commande): lance la classe `Main` du projet `sbt`

- `compile` (ou `sbt compile` hors de l'invite de commande): lance la compilation de l'ensemble du projet `sbt` (compile toutes les classes)

- `console` (`sbt console` hors de l'invite de commande): lance un REPL interactif Scala. Les dépendances du projet `sbt` seront disponibles et pourront être importés.

## Manipulation de fichiers

Nous allons voir ici quelques commandes pour vous aider avec la manipulation de fichiers en `Scala`. 

Pour lire un fichier nous pouvons le faire comme suit (en utilisant la lib [better-files](https://github.com/pathikrit/better-files)):

```scala
import better.files._

val f = File("/User/johndoe/Documents") // using constructor

// on va récupérer toutes les lignes du fichier
f.lines.toList

// si on veut récupérer tout le contenu du fichier en String
f.contentAsString
```

Pour écrire dans un fichier, nous pouvons le faire ainsi:

```scala
import better.files._

val f = File("/User/johndoe/Documents") // using constructor

// pour ajouter du contenu dans un fichier ligne par ligne
f.createIfNotExists()
  .appendLine() // on rajoute une ligne vide
  .appendLines("My name is", "Inigo Montoya") // on ajoute 2 nouvelles lignes

// pour écraser le contenu du fichier
f.createIfNotExists().overwrite("hello")
```

## Tests unitaires

Il est possible de lancer tous les tests du projets avec la commande: `sbt test` (ou `test` si on est dans l'invite de commande `sbt`).

Pour créer une classe de test, il suffit de créer une classe étendant `org.scalatest.funsuite.AnyFunSuite`:

```scala

import org.scalatest.funsuite.AnyFunSuite

class HelloSpec extends AnyFunSuite {}
```

Les tests devant être lancés doivent être placés dans le corps de la classe. Pour créer un test, il suffit d'appeler `test` en lui passant un nom de test et le code de test à effectuer comme ceci:

```scala
import org.scalatest.funsuite.AnyFunSuite

class HelloSpec extends AnyFunSuite {
  test("The Hello object should say hello") {
    assert(Hello.greeting === "hello")
  }
}
```

Le test sera lancé dès lorsqu'on lancera la commande `test`:

```scala
sbt:funprog-AL> test
[info] Formatting 1 Scala sources...
[info] compiling 1 Scala source to ../projet/funprog-al/target/scala-2.13/test-classes ...
[info] HelloSpec:
[info] - The Hello object should say hello
[info] Run completed in 251 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success] Total time: 1 s, completed 14 nov. 2021 à 14:46:48
```

Une classe de test d'exemple vous est fourni dans `./src/test/example/HelloSpec.scala`.

## Lecture des fichiers de conf

La librairie [config](https://github.com/lightbend/config) a été rajouté au projet. Elle permet de lire les fichiers de configuration (au format `.conf`).
Un fichier de configuration a éta ajouté au projet (voir le fichier `./src/main/resources/application.conf`).

Voici un exemple d'utilisation de l'api `config` (tiré de la documentation officielle).

Pour le fichier `application.conf` suivant:

```conf
foo {
  bar = 1
}

foo1.baz = "some texte"
foo1.baz = ${?FOO1_BAZ} # variable d'environnement pour sucharger la conf
```

La lecture du fichier de conf se fera comme suit:

```scala
import com.typesafe.config.{Config, ConfigFactory}

// Pour charger la configuration. `ConfigFactory#load` va chercher et lire le fichier `application.conf`.
val conf: Config = ConfigFactory.load()

// Une fois le fichier de conf chargé, on peut récupéré une valeur par sa clé (ex: `foo.bar`).
val bar1: Int = conf.getInt("foo.bar")
val foo: Config = conf.getConfig("foo")
val bar2: Int = foo.getInt("bar")
val baz: String = conf.getString("foo1.baz")
```
