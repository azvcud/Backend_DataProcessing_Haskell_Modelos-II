# Haskell Data Processing - Modelos de Programación II
## Universidad Distrital Francisco José de Caldas
### Proyecto Curricular de Ingeniería de Sistemas

**Asignatura:** Modelos de Programación II  
**Profesor:** Alejandro Paolo Daza Corredor

### Integrantes

- Amir Zoleyt Vanegas Cárdenas - 20211020015 <br> azvanegasc@udistrital.edu.co
- Esteban Alejandro Villalba Delgadillo - 20212020064 <br> eavillalbad@udistrital.edu.co
- Samuel Antonio Sanchez Peña - 20212020151 <br> samasanchezp@udistrital.edu.co

---

## Descripción del Proyecto

Este módulo del proyecto se encarga del procesamiento y análisis de datos en Haskell. Recibe la información obtenida desde el backend de Node.js, que recopila datos de la Premier League a través de APIs, y la procesa utilizando estructuras de datos funcionales. Los datos analizados incluyen resultados de partidos, estadísticas detalladas, tablas de posiciones y enfrentamientos directos.

El objetivo principal de esta implementación en Haskell es aprovechar las ventajas de la programación funcional para realizar consultas eficientes y estructuradas sobre la información recolectada.

---

## Características del Proyecto

- Procesamiento Funcional: Se emplean estructuras inmutables y funciones puras para transformar los datos.

- Estructura Modular: La implementación está dividida en módulos independientes para facilitar la extensión y el mantenimiento.

- Interfaz JSON: Se utiliza aeson para el manejo de datos JSON.

- Extracción de Datos: Diferentes módulos se encargan de procesar distintos tipos de datos provenientes de las APIs.

- Consulta de Datos: Se pueden realizar consultas filtradas y transformaciones de la información.

---

## Estructura del Proyecto

El proyecto está organizado de la siguiente manera:
```bash
Haskell_DataProcessing_Modelos-II/
├── app/
│   ├── Data/
│   │   ├── JSONMatchType.hs          # Definiciones de datos JSON para partidos
│   │   ├── JSONOtherType.hs          # Definiciones de otros tipos JSON
│   ├── FetchData/
│   │   ├── ClubApi.hs                # Obtención de datos de clubes
│   │   ├── MatchStaticsAPI.hs        # Obtención de estadísticas de partidos
│   │   ├── PremierLeagueTableAPI.hs  # Obtención de la tabla de posiciones
│   │   ├── ScheduleAPI.hs            # Obtención del cronograma de partidos
│   ├── Endpoints.hs                  # Definición de los endpoints de la API
│   ├── FootballAPI.hs                # Lógica principal de la API de fútbol
│   ├── Main.hs                       # Punto de entrada del sistema
├── CHANGELOG.md                      # Configuración del proyecto
├── LICENSE                           # Licensia del proyecto
├── hie.yaml                          # Configuración de Stack
├── README.md                         # Documentación del proyecto
```

## Modulos principales

- **Data/**

    - Contiene los tipos de datos JSON utilizados en el procesamiento de la información.

- **FetchData/**

    - ClubApi.hs: Procesa información de los clubes.

    - MatchStaticsAPI.hs: Extrae y analiza estadísticas de partidos.

    - PremierLeagueTableAPI.hs: Obtiene y organiza la tabla de posiciones.

    - ScheduleAPI.hs: Maneja los datos de programación de partidos.

- **Endpoints.hs**

    - Define los diferentes endpoints de la API para consultar la información procesada.

- **FootballAPI.hs**

    - Contiene la lógica principal de procesamiento y transformación de datos.

- **Main.hs**

    - Punto de entrada del servidor Haskell. Inicia la API y gestiona las solicitudes.

---
## Endpoints disponibles

- **Club Data: /club**

    - Devuelve información sobre los clubes de la Premier League.

- **Match Statistics: /match_stats**

    - Devuelve estadísticas detalladas de partidos.

- **Premier League Table: /premier_table**

    - Devuelve la tabla de posiciones actualizada.

- **Schedule: /schedule**

    - Devuelve el cronograma de partidos programados.

Todos los endpoints responden en formato JSON y permiten consultas filtradas.

## Diagramas

### Diagrama de Arquitectura

A continuación se presenta el diagrama de arquitectura del sistema, que describe la interacción entre los diferentes módulos y cómo Haskell procesa la información obtenida desde el backend en Node.js.

![Diagrama de Arquitectura](Diagramas/Diagrama_de_arquitectura.png)

---
## Instrucciones de Ejecución

### Requisitos Previos

- GHC (Glasgow Haskell Compiler)

- Stack

### Pasos para Ejecutar el Proyecto

1. Clonar el repositorio:
```bash
git clone https://github.com/tu-usuario/Haskell_DataProcessing_Modelos-II.git
```
2. Navegar al directorio del proyecto:
```bash
cd Haskell_DataProcessing_Modelos-II
```
3. Instalar dependencias:
```bash
stack setup
stack build
```
4. Ejecutar el servidor:
```bash
stack run
```
5. Acceder a los endpoints: Se puede usar un navegador o herramientas como curl o Postman.
```bash
curl http://localhost:8080/premier_table
```
---

## Ejemplo de Uso

### Obtener Fixtures (Partidos Programados)

**URL:** `http://localhost:3000/fixtures`

**Respuesta:**

```json
[
  {
    "homeTeam": "Chelsea",
    "awayTeam": "Manchester United",
    "date": "2023-10-15T14:00:00Z",
    "status": "Scheduled"
  },
  {
    "homeTeam": "Liverpool",
    "awayTeam": "Arsenal",
    "date": "2023-10-16T16:30:00Z",
    "status": "Scheduled"
  }
]