# -*- coding: utf-8 -*-
"""
Created on Wed Aug 21 16:02:25 2019

@author: grant
"""

import numpy as np

#Setting up initial conditions
board = np.array([[' ',' ',' '],
                  [' ',' ',' '],
                  [' ',' ',' ']])
example_board = np.array([['A1','B1','C1'],
                          ['A2','B2','C2'],
                          ['A3','B3','C3']])
turn = 'X'
end = False

#Printing initial board state
print("These are the board positions:")
print(example_board[0])
print(example_board[1])
print(example_board[2])
print("This is the initial board state:")
print(board[0])
print(board[1])
print(board[2])

#Playing game
while end == False:
    #Getting input from player
    while True:
        try:
            position = input("Player " + turn + ", choose a position: " )
            result = np.where(example_board == position)
            coordinate= tuple(zip(result[0], result[1]))
            if position not in example_board:
                raise NameError
            elif board[coordinate[0]] != ' ':
                raise ValueError
            break
        except NameError:
            print('That is not a valid position. Try Again...')
        except ValueError:
            print('That spot has already been taken. Try Again...')

    result = np.where(example_board == position)
    coordinate= tuple(zip(result[0], result[1]))
    board[coordinate[0]] = turn
    
    #Printing board state
    print("This is the current board state:")
    print(board[0])
    print(board[1])
    print(board[2])
    
    #Checking to see if player has won
    #Checking all rows
    if np.any(np.all(board == turn, axis=0)):
        end = True

    #Checking all columns
    elif np.any(np.all(board == turn, axis=1)):
        end = True
    
    #Checking diagonals
    elif board[0][0] == turn and board[1][1] == turn and board[2][2] == turn:
        end = True
    elif board[0][2] == turn and board[1][1] == turn and board[2][0] == turn:
        end = True

    #Printing winner
    if end == True:
        print("Player " + turn + " wins!")
        
    #Checking to see if board is full
    if np.all(board != ' '):
        print("It's a tie!")
        break

    #Switching turns
    if turn == 'X':
        turn = 'O'
    elif turn == 'O':
        turn = 'X'