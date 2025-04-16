# Simple Water for s&box

> [!WARNING]  
> Still work-in-progress, only works for scenes (not hammer maps)

https://github.com/user-attachments/assets/dec2308f-289b-4975-a2a3-c98353c5cb7d

## Features

- Dynamic reflections, with options for both anti-aliased and aliased, with cubemap fallback
- Caustics (either texture- or flipbook-based), projected onto surface below, can be blurred and has chromatic aberration
- Runtime generated procedural normals (1 to 4 layers) or texture-driven normals (2 to 4 layers)
- Depth-based fog
- Normal & refraction scale
- Refraction and reflection blur (both tied to each other - designed to be used for cloudy liquids, like horrible green swamp water)
