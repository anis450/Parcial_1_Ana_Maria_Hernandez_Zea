# Comparación de Paradigmas de Programación: Imperativo (Python) vs Declarativo/Funcional (Haskell)

Este documento presenta un análisis comparativo entre la programación imperativa (ejemplo en Python) y la programación declarativa/funcional (ejemplo en Haskell), aplicado a un problema de gestión y ordenamiento de estudiantes.

---

## Ejemplo en Python (Imperativo)

En el paradigma imperativo se indica explícitamente cómo se realizan los pasos para ordenar los registros.

```python
# Lista de estudiantes (nombre, nota)
estudiantes = [
    ("Daniela", 90),
    ("Carmen", 85),
    ("Lottie", 90),
    ("Juan", 75)
]

# Ordenamiento manual con bucles e intercambio
for i in range(len(estudiantes)):
    for j in range(i + 1, len(estudiantes)):
        if (estudiantes[i][1] < estudiantes[j][1]) or (
            estudiantes[i][1] == estudiantes[j][1] and estudiantes[i][0] > estudiantes[j][0]
        ):
            estudiantes[i], estudiantes[j] = estudiantes[j], estudiantes[i]

print("Imperativo:", estudiantes)
```

---

## Ejemplo en Haskell (Declarativo/Funcional)

En el paradigma declarativo/funcional se describe qué se quiere lograr, y el lenguaje se encarga del cómo.

```haskell
import Data.List (sortBy)
import Data.Ord (comparing)

-- Lista de estudiantes (nombre, nota)
estudiantes :: [(String, Int)]
estudiantes =
    [ ("Ana", 90)
    , ("Carlos", 85)
    , ("Beatriz", 90)
    , ("Daniel", 75)
    ]

-- Ordenar: primero por nota descendente, luego por nombre ascendente
ordenados :: [(String, Int)]
ordenados =
    sortBy (comparing (\(nombre, nota) -> (-nota, nombre))) estudiantes

main :: IO ()
main = do
    putStrLn "Declarativo:"
    print ordenados
```

---

## Análisis Comparativo

### Claridad y legibilidad
- **Python (imperativo):** se requiere detallar todos los pasos del ordenamiento con bucles e intercambios.
- **Haskell (funcional):** el uso de `sortBy` y `comparing` da una solución más corta y expresiva.

### Nivel de expresividad y abstracción
- **Python:** describe paso a paso el procedimiento.
- **Haskell:** describe directamente el criterio de ordenamiento.

### Manejo de estructuras de datos (mutabilidad vs inmutabilidad)
- **Python:** la lista original se modifica (mutable).
- **Haskell:** las listas son inmutables, se genera una nueva lista ordenada.

### Manejo de estado
- **Python:** el estado cambia en cada iteración del bucle.
- **Haskell:** no hay cambios de estado, solo transformaciones de datos.

### Facilidad de mantenimiento y extensión
- **Python:** cambiar el criterio de ordenamiento implica modificar condiciones dentro de los bucles.
- **Haskell:** basta con cambiar la función usada en `comparing`.

### Eficiencia
- **Python:** depende de la implementación manual (en este caso burbuja, no tan eficiente).
- **Haskell:** usa algoritmos de ordenamiento optimizados de librería estándar.

---

## Conclusión

El paradigma imperativo (Python) ofrece control detallado sobre el proceso, pero requiere más pasos para implementar.  
El paradigma funcional/declarativo (Haskell) es más expresivo y conciso, sacrificando control por simplicidad y claridad.
