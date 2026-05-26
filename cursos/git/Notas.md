# Que es Git

Un software de código abierto que permite tener trazabilidad sobre el desarrollo del código, capturando el conjunto de archivos que componen un desarrollo para un momento dado.  
Esto garantizá control sobre los cambios y su impacto sobre la funcionalidad del desarrollo, asi como sobre el origen de los cambios (quien los realizó).

## Descarga de Git

Directamente en el [enlace de descarga](https://git-scm.com/), que es la página principal de **Git** en **Google**.

## Creación de un repositorio

Para esto necesitamos conocer las funciones básicas de consola y haber definido un espacio conocido como **Directorio de Trabajo (WD)** que contiene todos los archivos que hacen parte del proyecto.

- `git init`: Solo se utiliza al iniciar un nuevo proyecto.
    - Esto define un *Área de ensayo* y un *Repositorio local*.
- `git add`: Se utiliza para incorporar archivos al seguimiento.
    - Le pedimos a Git que haga seguimiento de un conjunto (o de la totalidad) de archivos en el WD, con esto lo traslada al área de ensayo.
- `git commit`: Toma de instantánea.
    - Con este comando traslada el archivo al repositorio local y captura la instantanea.
- Recurso recomendado de comandos [Complete list of all comands](https://git-scm.com/docs/git#_git_commands)

 ###  Ejecución práctica

 1. Creación de un WD que contenga los archivos de interés.
 2. Click derecho sobre la carpeta y seleccionamos `git bash here`
      - Esto abre una consola o terminal
      - Ejecutamos `git init`
 4. El resultado es la creación de una carpeta oculta de nombre `.git`

### Consulta de estado de proyecto

¿Como puedo conocer los archivos que están vinculados al proyecto Git?

- `git status -s`: Lista los archivos aún no vinculados
   - Ej: `?? index.qmd`

### Captura del estado del proyecto en un instante específico

¿Como puedo almacenar el proyecto en un punto determinado?

Siempre va primero el `add` y después el `commit`

1. `git add index.qmd` Añade el archivo al seguimiento
2. `git commit -m "Mi primer commit"` Ejecuta la captura

### Consulta de los cambios realizados (historial)

¿Como puedo conocer los `commit` realizados a lo largo del proceso?

- `git log --oneline`: Muestra los commit realizados con la descripción insertada.

### Restaurar el proyecto a una captura previa

¿Como puedo hacer que el proyecto retorne a una captura previa?

- `git reset --hard 2533360`: Restaura el proyecto al instante almacenado con el código posterior a "hard"

### Añadir todos los archivos al seguimiento

¿Como puedo vincular todos los archivos de la carpeta al seguimiento del proyecto?

- `git add .`: Con el punto despues del `add` se incluye la totalidad de los archivos al seguimiento.

### Atajo para realizar un add y un commit al mismo tiempo 

- `git commit -am "Descripción commit"`: Con este comando no es necesario separar los add de los commit.

### Modificar la descripción de un commit

1. abrir el editor vim
2. Ejecutar el comando `:i`
3. Suprimir la descripción existente
4. Ejecutar el comando `:i`
5. Escribir la nueva descripción + Enter
6. Ejecutar el comando `:wq`

## Cargar el proyecto a `GitHub`

1. Crear un repositorio en GitHub
2. Ejecutar la primera y tercera línea del repositorio creado:
    - `git remote add origin https://github.com/GomezGerardoEsteban/CursoGit.git`
    - `git push -u origin main`

### Cambios en remoto que deben actualizarse en local

Si se generó un cambio en remoto, es necesario copiar esos cambios a los archivos locales, eso se realiza con el comando `git pull`. No hace falta especificar nada más.

## Generación de `tags`

Los `tags` son fotografías completas de los proyectos a un momento dado, permite que desde GitHub se puedan descargar todos los archivos vinculados al proyecto. Para generarlos se utiliza:

- `git push --tags`: En el inicio del proyecto en GitHub, está la pestaña de `tags` que indica cuantas versiones completas del proyecto se han ejecutado.

## Clonación de repositorios de GitHub

Si quieres copiar en local un proyecto que esta almacenado en GitHub.

1. Copias la ruta del proyecto
2. Ejecutas en el bash local `git clone ruta`
