# Super Lambda Bros.
Jumping action game on Haskell.

![Game Icon](images\icon.png)

## Authors
* Denis Chernikov
* Maxim Surkov
* Vladislav Kuleykin

## Purposes
This project was done during the *Programming in Haskell* course (primary instructor: *Nickolay Kudasov*) at *Fall 2018* semester at *Innopolis University* (Innopolis, Russia).

## How to run
Run the next command to build the project, it can take some time, as it downloads all dependencies:
```cmd
> stack build
```

After you built it once, you can run the game:
```cmd
> stack run
```
You'll be greeted with the number of players selection screen, choose the needed amount using `W` and `D` buttons, then press `Enter` and play!

Current settings for players movement:
* Player 1 - W A S D
* Player 2 - ↑ ← ↓ →
* Player 3 -  U H J K
But they can be changed in `src/Handle.hs` file, by changing the `G.Char` or `G.SpecialKey` for the needed action`P{player_num}_{direction}_BUTTON` in `handleGame` function.

To exit the game press the `ESC` button.

## Screenshots

<!-- Скриншота/гифки нет. -->

## Implementation

### What works
* **Animation** - the animation of players, enemies and coins are working;
* **Basic game mechanics** - physics, enemy killing, moving inertia, moving between levels;
* **Easy level creation/adding** - all the maps are in `assets/maps` folder and are in convenient `txt` format, to add new layers, just add a `map_{level_num}.txt` map file and update the counter in `src/App.hs` file `map show [1..{n_levels} :: Integer])`;
* **Local multiplayer** - local multiplayer up to 3 players (fully customizable), the screen is ajusted to be in the center of their locations, and you can't leave the screen.

### What doesn't work
Everything else 😅, like:
* Invincible state during the shrinkage;
* Collision avoidance - when you stuck at the corner of the block, the game should push you out;
* Proper UI;
* Not all animations are present (dying, winning, etc.);
* Not all mechanics from the first level are implemented (sitting, turtles, stars);
* Pipes;
* Proper physics - maybe avoid floating point numbers?

## Contribution
Feel free to contribute to this project. We'll be glad for your help.


## Copyright notes
All the media content (including graphics & music) is created and owned by `Nintendo Company Ltd.`, the original game produced by this company is called `Super Mario Bros.` (1985). All rights reserved.

## Related materials
* Very detailed description of the original game – https://themushroomkingdom.net/smb_breakdown.shtml
* Original game maps – https://themushroomkingdom.net/maps/smb
* Original game tiles and sprites – http://www.mariomayhem.com/downloads/sprites/super_mario_bros_sprites.php
* Original game animations – https://themushroomkingdom.net/media/smb/anigifs
* Original game music – https://themushroomkingdom.net/media/smb/mid
* Original game SFX – https://themushroomkingdom.net/media/smb/wav
* Original game emulation for comparison – http://emulator.online/nes/super-mario-bros/