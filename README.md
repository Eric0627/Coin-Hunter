# Coin-Hunter
The final project for CSE 230 FA23

Collaborators:
* Chaohui Li
* Han Chen
* Hongzhang Shi
* Nantong Chen

## Description
### Game Rules
Coin Hunter is a networked multiplayer game implemented in Haskell. The game is set in a maze with coins, obstacles, and ghosts. Each player operates a character to navigate through the maze, collect as many coins as possible, and reach the exit within the time limit. When walking through the maze, players should try to avoid obstacles and ghosts to save time. The final score for ranking is calculated based on the coins collected and the total time spent. If there are multiple players, they can either compete individually or group in two teams. If they play in two groups, the group with a higher total score wins. 

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


