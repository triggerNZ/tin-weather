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

Overall goals
-------------
- Demonstrate my programming ability
- Simulate weather in a simple but plausible way but also to discover some abstractions which could be used for
more serious weather simulations
- Weather is complex. Everything affects everything. Can we express simple relationships and compose them into
more comlex ones?
- Can we tweak parameters and chop and change rules to simulate other weather, for example on other planets?
- I believe these goals were achieved.


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

This is roughly the direction the data flows. `A => A'` indicates a value updating itself over a step.

```
Nasa Image                                     => Elevation
Elevation                                      => Terrain
(Coordinates, Time)                            => Solar Radiation
(Cloud, Temperature)                           => (Precipitation, Cloud')
(Temperature, Terrain, Solar Radiation, Cloud) => Temperature'
(Elevation, Temperature)                       => Pressure
(Humidity, Temperature, Terrain)               => Humidity'
(Cloud, Humidity)                              => (Cloud', Humidity')

```

On top of that there is convection, described below.

### Convection

To describe convection, I exploited the comonadic structure of a cursor. The main convection function is 
`Convection.convection`. It takes a `GlobeCursor`, and looks at the pressure differentials in each direction
to compute the new parameters at the current cursor. `cobind` allows us to automatically apply this transformation
(lazily of course) to the whole globe.

---

### Limitations and potential solutions

- The simple equations used to model weather look good over a few iterations but struggle to stay away from
extreme temperature/pressure etc. values over a long sequence of iterations. Solutions:
    - Better modelling of the relationships involved (this is essentially meteorology)
    - Tweaking constants. The slightest change can really vary long term results. There is a climate change lesson here.
- The comonadic structure of convection does not play well with the lazy structure of globes.
    - We can still simulate a few days, but because computing 1 current pixel requires 5 previous step pixels,
    the process is exponential (5^n) in the number of steps. This is for infeasible simulating months or years.
    A few days is fine. The main program comes with convection turned off by default for this reason. Possible solutions:
        - Periodically materialise the whole map into an eager globe. (Say every 24 hours). With a lot of compute, this 
        be reasonable but I ran out of patience rendering a single materialised view on my Macbook Pro. There are likely
        optimisations there could make this much faster with more time
        - Use other data as sources of convection (currents, winds, etc) rather than computing it from pressure differentials.