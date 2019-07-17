Weather simulation
==================

Running
-------

To generate the output table:

```
sbt run
```

will generate the basic output table without convection for 365 days in 3 hour steps.

```
sbt "run --convection <num-days>" 
```
will generate the output table WITH convection for `num-days` days in 3 hour steps. Note that num-days should be small 
(1 - 5) for reasons explained below.

---

To generate demo images:

```
sbt "run --demoImage <demo-name>"
```

Where `demo-name` is one of `elevations`, `elevationsMapped`, `sinSunshine`, `sinSunshineWithElevations`, `sea`.
These were not part of the assignment but were useful for debugging and are nifty so I kept them in. Be patient
when generating these, the globe structure is not optimised for generating every pixel, which we do here.

Only the components in the early parts of the pipeline can be rendered. For later stages, (like actual weather),
it is computationally infeasible.

---

Core Abstractions
-----------------

### Globe

A `Globe[A]` is a data structure representing values of `A` distributed around a sphere. It is applicative 
but not monadic. All weather states are represented as globes. The final representation of weather is represented
as a `Globe[(Temperature, Pressure, Humidity, Cloud, Precipitation)]`.

##### Laziness

Globes have two variants - eager and lazy. Eager contains a raw array of `A`s and is used only when converting the 
elevation image buffer to a globe. All of the remaining operations are lazy. Instead of storing data, they contain
`(coordinate) => A` functions. This allows us to represent values across the globe without actually computing all of
them. That is a good design for this assignment since we model the whole earth but we actually only look up a few cities.
The functions themselves can be fairly complex (as climate models like to be) because they are only called by need.

### GlobeCursor

A `GlobeCursor[A]` is an index into a `Globe[A]`. It supports moving north, south, east and west, and is comonadic.
This allows us to represent operations on whole globes elegantly in terms of local operations:

```
globe.cobind(cursor => B).globe gives us a Globe[B]
```

### Data dependencies

```
Nasa Image => Elevations

```