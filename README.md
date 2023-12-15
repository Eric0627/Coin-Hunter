# Coin-Hunter
The final project for CSE 230 FA23

Collaborators:
* Chaohui Li
* Han Chen
* Hongzhang Shi
* Nantong Chen

## Setup
### Installation (default: the dev branch)
```
git clone git@github.com:Eric0627/Coin-Hunter.git
```
### Compilation
```
cabal build
```
### Run (recommended: FiraCode Nerd Font for terminal)
* Server
```
cabal run coin-hunter
```
* Client (set up the IP address and port of the server)
```
cabal run Client
```

## Description
### Game Rules
Coin Hunter is a networked multiplayer game implemented in Haskell. The game is set in a maze with coins, obstacles, and ghosts. Each player operates a character to navigate through the maze, collect as many coins as possible, and reach the exit within the time limit. When walking through the maze, players should try to avoid obstacles and ghosts to save time. The final score for ranking is calculated based on the coins collected and the total time spent. If there are multiple players, they can either compete individually or group in two teams. If they play in two groups, the group with a higher total score wins. 

![alt text](https://github.com/Eric0627/Coin-Hunter/blob/dev/quick_demo.gif)
### Features
* Both single and multiplayer modes are provided. 
* Mazes with obstacles and ghosts can be automatically generated. 
* Players can customize the difficulty level by adjusting the number of obstacles and speed of ghosts.
* A ranking system is provided and history scores can be recorded.

### Goals
* Define various objects, including characters, coins, obstacles, and ghosts.
* Develop a command line interface for colorfully displaying objects. 
* Design the movement logic of characters and ghosts.
* Design an algorithm for generating mazes.
* Implement server and client end to support networked play.
* Design a ranking and record system. 

### Useful Links
* Haskell terminal user interface (TUI) programming toolkit (https://github.com/jtdaugherty/brick/)
* Network socket library for Haskell programming (https://hackage.haskell.org/package/network-3.1.4.0/docs/Network-Socket.html)

## Updates
### Architecture
The architecture of Coin Hunter is based on the framework provided by [brick](https://github.com/jtdaugherty/brick/) with reference to the brick-based maze game. We have a mazeApp instance that encapsulates functions for initializing a game state, drawing the user interface, defining styles, and handling various events. A game state consists of the maze shape, the player position, the coin positions, the current time, the number of coins collected, etc. It evolves over the course of the game's execution. Starting with an initial game state, the event handler keeps listening to different kinds of events. For example, the keystrokes from users are monitored to move the character during the game. Besides, the elapsing time is recorded to dynamically update the total used time. When the character meets a coin in the maze, an event of collecting coins is captured to update the number of collected coins and delete the coin in the maze. If the character arrives at the exit, the game will end with a finished state, and the final time and coin number will be displayed. If multiple players are connected through the network, several characters will be created, and the game state will record the positions of all characters. Any event from each player can change the game state, and the game will not end until all characters reach the exit. 

### Challenges
We had challenges when designing the method for randomly generating coins. Basically, a coin corresponds to a coordinate in the maze, and we can generate a list of all possible coordinates from the maze shape. However, it is difficult to define a function for randomly sampling some coordinates from the list. After searching on hoogle and reading some documentation, we utilized the global pseudo-random number generator and the random function from the System.Random library to implement our sampling function. As a result, when a new game is created, a random seed will given to our sampling function so that a random collection of coins will be generated in the maze. Currently, we still have the following challenges to be resolved:
* Generating a maze with obstacles and moving monsters
* Handling events in the multiplayer mode
* Creating network sockets for multiple players

### Expectation
We will meet our goals before the deadline, although our progress is a little bit slow. If time is too limited, we may discard some functions, like the moving monsters. However, we will try our best to collaborate well with good job distribution and time management. 

## Final Updates
* Implemented the server and client ends
* Realized the multiplayer mode
* Updated the UI
* Added quick testing

