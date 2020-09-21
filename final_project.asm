;------------------------------------------------------------------------------------------------
; Program :                               
; ------------
; This program creates a regular maze in the beggining of its operation.
; It's creates regular maze if you press '1'. It create Random maze if you press '2'.
; You need to move the player from the start to End.
;------------------------------------------------------------------------------------------------


DIR_UTD = 1                             ; Constant definition.
DIR_LTR = 2                             ; Constant definition.
                                        
.model large                            ; Define the program as large.
.stack 1000h                            ; Define stack in the size of 1000h
.data                                   ; Define the data segment.
    cord_X        db         00h        ; Define cord_X as 0.
    cord_Y        db         00h        ; Define cord_Y as 0.
    start_cap     db    'START$'        ; Define start_cap as 'START$'.
    end_cap       db      'END$'        ; Define end_cap as 'END$'.
    Maze          db 1000 dup(1)        ; Define Maze as an array of 1.
    Maze2         db 1000 dup(1)        ; Define Maze2 as an array of 1.
    Location_X    dw         00h        ; Define Location_X as 0.
    Location_Y    dw         00h        ; Define Location_Y as 0.
    Length        db         00h        ; Define Length as 0.
    Direction     db         00h        ; Define Direction as 0.
    Player_X      db         00h        ; Define Player_X as 0.
    Player_Y      db         01h        ; Define Player_Y as 0.
    Pixel_data    db   64 dup(?)        ; Define Pixel_data as Array of 64 value ?.
    Pixel_Pointer dw       0000h        ; Define Pixel_Pointer as 0.
    RandomNumber  dw       0000h        ; Define Random Number as 0.
    Remember_X    db           0
    Remember_Y    db           0 
    Blue_Around   db           0                          
.code                                   ; Define Code Segment.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
    mov AH, 2CH                         ; Get the current system time.
    mov AL, 0                           ; Move 0 to AL register.
    int 21H                             ; Activate the Interrupt.
    
    mov ax, cx                          ; Move the time data into AX register.
    inc ax                              ; Increment AX register.
    inc dx                              ; Increment DX register.
    mul dx                              ; Multiply AX register By DX register.

    mov RandomNumber, ax
    
    call ResetG                         ; Call the ResetG subroutine.
    
; ===============================================================================================
; ======================                                                   ======================             
; ======================         F u n c t i o n s      S e c t i o n      ======================  
; ======================                                                   ======================                                      
; ===============================================================================================

;------------------------------------------------------------------------------------------------
; Function Name		    : ResetG
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Creates a regular maze.
;------------------------------------------------------------------------------------------------
  
ResetG:                               
    call DefineDataSegment              ; Call the DefineDatSegment subroutine.
    call CreateGame                     ; Call the CreateGame subroutine.
    call WaitingKeyPress                ; Call the WaitingKeyPress subroutine.
    call EndOfProgram                   ; Call the DefineDataSegment subroutine.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : RandomGame
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Create a Random maze.
;------------------------------------------------------------------------------------------------
 
RandomGame:                            
    call DefineDataSegment              ; Call the DefineDataSegment subroutine.
    call CreateRandomGame               ; Call the CreateRandomGame subroutine.
    call WaitingKeyPress                ; Call the WaitingKeyPress subroutine.
    call EndOfProgram                   ; Call the EndOfProgram subroutine.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : CreateRandomGame
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Create the Map of the random game.
;------------------------------------------------------------------------------------------------

CreateRandomGame:                       
    call ResetVariables                 ; Call the ResetVariables subroutine.
    call DefineGraphicScreen            ; Call the DefineGraphicScreen subroutine.
    call ColorScreen                    ; Call the ColorScreen subroutine.
    call DefineRandomScreen             ; Call the DefineRandomScreen subroutine.
    call CreateMatrix                   ; Call the CreateMatrix subroutine.
    call Print_Start                    ; Call the Print_Start subroutine.
    call Print_End                      ; Call the Print_End subroutine.
    call ClearBlack                     ; Call the ClearBlack subroutine.
    call SetPlayer                      ; Call the SetPlayer subroutine.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : DefineRandomScreen
; Function Inputs		: variable RandomNumber.
; Function Outputs		: None.                               
; Function Operation	: Draw the random maze.
;------------------------------------------------------------------------------------------------

DefineRandomScreen:                     
   mov ax, RandomNumber                 ; Get the random number.
   mov bx, 7                            ; Move 7 to BX.
   mul bx                               ; Multiply AX by BX.
   add ax, 1                            ; Add 1 to AX.
   mov RandomNumber, ax                 ; Save the RandomNumber.                   
   mov bx, 3                            ; Move 3 to BX.
   div bx                               ; Divide AX by BX.
   
   cmp dx, 0                            ; Check if DX equal 0.
   je DefineDefaultScreen               ; Default.
   cmp dx, 1                            ; Check if DX equal 1.
   je DefineRandomScreen1               ; Random 1.
   cmp dx, 2                            ; Check if DX equal 2.
   je DefineRandomScreen2               ; Random 2.
   jmp DefineDefaultScreen              ; Default.
ret
 
;------------------------------------------------------------------------------------------------
; Function Name		    : WaitingKeyPress
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Waits for the user to press the keyboard and do operations in response
;                         to the input.
;------------------------------------------------------------------------------------------------
                                        
WaitingKeyPress:                                                               
IsPressed:                              ; IsPressed
    
    cmp Player_X, 39                    ; Check if the player reach the End
    jne con_prog1
    cmp Player_Y, 23
    jne con_prog1
    
    jmp EndOfProgram                    ; If so, End the program.
    
    con_prog1:
    in   al, 64h                        ; Check if a key was pressed.
    and  al, 01h                        ; Leave only the LSB. 0000 000X
    jnz  IsPressed                      ; If key wasnt pressed, return to IsPressed.
                                        
    mov  ah, 0                          ; Zero to the AH register.
    in   al, 60h                        ; Get the pressed key from the keyboard.
                                                                                             
    cmp  al, 01h                        ; Checks if the 'ESC' key was Pressed.
    je   EndOfProgram                   ; If so, Exit the program.
                                                                                 ;
    cmp  al, 02h                        ; Check if a key "Lower" than the 1-key was press.
    je   reset ; Regular Game           ; If so, return to the PressCheck.
                                        
    cmp  al, 03h                        ; Check if a key "Greater" than the 9-key was press.
    je   random_g ; Random Game         ; If so, return to the PressCheck.
                                        
    cmp  al, 48h                        ; Check if a the "Up" arrow was pressed.
    je   load_up                        ; If so, do load_up.
                                        
    cmp  al, 50h                        ; Check if a the "Down" arrow was pressed.
    je   load_down                      ; if so, do load_down.
                                        
    cmp  al, 04Bh                       ; Check if a the "Left" arrow was pressed.
    je   load_left                      ; If so, do load_left.
                                        
    cmp  al, 04Dh                       ; Check if a the "Right" arrow was pressed.
    je   load_right                     ; If so, do load_right.
                                        
    jmp  redo_sub                       ; Redo the sub.
                                        
    random_g:                           ; random_g
    call RandomGame                     ; Call the RandomGame subroutine.
                                        
    reset:                              ; reset
    call ResetG                         ; Call the ResetG subroutine.
                                        
    load_up:                            ; load_up
    call MoveUp                         ; Call the MoveUp subroutine.
    jmp  redo_sub                       ; Jump to redo_sub.
                                        
    load_down:                          ; load_down
    call MoveDown                       ; Call the MoveDown subroutine.
    jmp  redo_sub                       ; Jump to redo_sub.
                                        
    load_left:                          ; load_left
    call MoveLeft                       ; Call the MoveLeft subroutine.
    jmp  redo_sub                       ; Jump to redo_sub.
                                        
    load_right:                         ; load_right
    call MoveRight                      ; Call the MoveRight subroutine.  
    redo_sub:                           ; redo_sub
    jmp  IsPressed                      ; If a number was pressed, jump to PlayKey.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : ResetVariables
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Reset all the Data Segment Variables.
;------------------------------------------------------------------------------------------------
                                        
ResetVariables:                         
    mov  cord_X,         00h            ; Move 0 into cord_X.
    mov  cord_Y,         00h            ; Move 0 into cord_Y.
    call ResetMaze                      ; Call the ResetMaze subroutine.
    call ResetMaze2                     ; Call the ResetMaze2 subroutine.
    mov  Location_X, 00h                ; Move 0 into Location_X.
    mov  Location_Y, 00h                ; Move 0 into Location_Y.
    mov  Length, 00h                    ; Move 0 into Length.
    mov  Direction, 00h                 ; Move 0 into Direction.
    mov  Player_X, 00h                  ; Move 0 into Player_X.
    mov  Player_Y, 01h                  ; Move 1 into Player_Y.
    mov  Pixel_Pointer, 0000h           ; Move 0 into Pixel_Pointer. 
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : ResetMaze
; Function Inputs		: None.
; Function Outputs		: Empty Maze array.                               
; Function Operation	: Empty the Maze array to its default value.
;------------------------------------------------------------------------------------------------

ResetMaze:                              
    mov si, 0                           ; Move 0 into SI register.
    reset_maze_cell:                    ; reset_maze_cell
        mov ds:Maze[si], 1              ; Move 1 into ds:Maze[si].
        add si, 1                       ; Add 1 to SI.
        cmp si, 1000                    ; Compare the SI register to 1000.
    jl reset_maze_cell                  ; Jump if Lower.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : ResetMaze2
; Function Inputs		: None.
; Function Outputs		: Empty Maze2 array.                               
; Function Operation	: Empty the Maze2 array to its default value.
;------------------------------------------------------------------------------------------------

ResetMaze2:                             
    mov si, 0                           ; Move 0 into SI register.
    reset_maze2_cell:                   ; reset_maze2_cell
        mov ds:Maze2[si], 1             ; Move 1 into ds:Maze2[si].
        add si, 1                       ; Add SI to 1.
        cmp si, 1000                    ; Compare the SI register to 1000.
    jl reset_maze2_cell                 ; Jump if Lower.
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : MoveUp
; Function Inputs		: Player_X, Player_Y - Players coordinates.
; Function Outputs		: None.                               
; Function Operation	: Move the player Up.
;------------------------------------------------------------------------------------------------

MoveUp:                                 
    call MatrixPosition                 ; Call the MatrixPosition subroutine.
    mov  bx, ax                         ; Move AX register into BX register.
    mov  al, ds:Maze[bx-40]             ; Move ds:Maze[bx-40] into AL register.
    cmp  al, 1                          ; Compare the AL register to 1.
    je   do_up                          ; Jump if Equal.
    ret                                 ; Return from the subroutine.
    do_up:                              ; do_up
    call LoadCell                       ; Call the LoadCell subroutine.
    sub  Player_Y, 1                    ; Sub 1 from Player_Y
    mov  al, ds:Player_X                ; Move ds:Player_X into AL.
    mov  bl, ds:Player_Y                ; Move ds:Player_Y into BL.
    mov  ds:cord_X, al                  ; Move AL into ds:cord_X.
    mov  ds:cord_Y, bl                  ; Move BL into ds:cord_Y.
    call SaveCell                       ; Call the SaveCell subroutine.
    call DrawCarUp                      ; Call the DrawCarUp subroutine.
    mov  bx, 10                         ; Move 10 into BX.
    call Wait_Sub                       ; Call Wait_Sub subroutine.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : MoveDown
; Function Inputs		: Player_X, Player_Y - Players coordinates.
; Function Outputs		: None.                               
; Function Operation	: Move the player down.
;------------------------------------------------------------------------------------------------

MoveDown:                               
    call MatrixPosition                 ; Call the MatrixPosition subroutine.
    mov  bx, ax                         ; Move AX into BX.
    mov  al, ds:Maze[bx+40]             ; Move ds:Maze[bx+40] into AL.
    cmp  al, 1                          ; Compare the AL register to 1.
    je   do_down                        ; Jump if equal.
    ret                                 ; Return from the subroutine.
    do_down:                            ; do_down
    call LoadCell                       ; Call the LoadCell subroutine.
    add  Player_Y, 1                    ; Add 1 to Player_Y.
    mov  al, ds:Player_X                ; Move ds:Player_X into AL.
    mov  bl, ds:Player_Y                ; Move ds:Player_Y into BL.
    mov  ds:cord_X, al                  ; Move AL into ds:cord_X.
    mov  ds:cord_Y, bl                  ; Move BL into ds:cord_Y.
    call SaveCell                       ; Call the SaveCell subroutine.
    call DrawCarDown                    ; Call the DrawCarDown subroutine.
    mov  bx, 10                         ; Move 10 Into bx.
    call Wait_Sub                       ; Call Wait_Sub subroutine.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : MoveLeft
; Function Inputs		: Player_X, Player_Y - Players coordinates.
; Function Outputs		: None.                               
; Function Operation	: Move the player Left.
;------------------------------------------------------------------------------------------------

MoveLeft:                               
    call MatrixPosition                 ; Call the MatrixPosition subroutine.
    mov  bx, ax                         ; Move AX Into bx.
    mov  al, ds:Maze[bx-1]              ; Move ds:Maze[bx-1] Into AL.
    cmp  al, 1                          ; Compare the AL register to 1.
    je   do_left                        ; Jump if equal.
    ret                                 ; Return form the subroutine.
    do_left:                            ; do_left
    call LoadCell                       ; Call the LoadCell subroutine.
    sub  Player_X, 1                    ; Sub 1 from Player_X .
    mov  al, ds:Player_X                ; Move ds:Player_X Into AL.
    mov  bl, ds:Player_Y                ; Move ds:Player_Y Into BL.
    mov  ds:cord_X, al                  ; Move AL Into ds:cord_X.
    mov  ds:cord_Y, bl                  ; Move BL Into ds:cord_Y.
    call SaveCell                       ; Call the SaveCell subroutine.
    call DrawCarLeft                    ; Call the DrawCarLeft subroutine.
    mov  bx, 10                         ; Move 10 Into BX.
    call Wait_Sub                       ; Call Wait_Sub subroutine.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : MoveRight
; Function Inputs		: Player_X, Player_Y - players coordinates.
; Function Outputs		: None.                               
; Function Operation	: Move the players Right.
;------------------------------------------------------------------------------------------------

MoveRight:                              
    call MatrixPosition                 ; Call the MatrixPosition subroutine.
    mov  bx, ax                         ; Move AX Into BX.
    mov  al, ds:Maze[bx+1]              ; Move ds:Maze[bx+1] Into AL.
    cmp  al, 1                          ; Compare the AL register to 1.
    je   do_right                       ; Jump if equal.
    ret                                 ; Return from the subroutine.
    do_right:                           ; do_right
    call LoadCell                       ; Call the LoadCell subroutine.
    add  Player_X, 1                    ; Add 1 to Player_X.
    mov  al, ds:Player_X                ; Move ds:Player_X Into AL.
    mov  bl, ds:Player_Y                ; Move ds:PLayer_Y Into BL.
    mov  ds:cord_X, al                  ; Move AL Into ds:cord_X.
    mov  ds:cord_Y, bl                  ; Move BL Into ds:cord_Y.
    call SaveCell                       ; Call the SaveCell subroutine.
    call DrawCarRight                   ; Call the DrawCarRight subroutine.
    mov  bx, 10                         ; Move 10 Into BX.
    call Wait_Sub                       ; Call Wait_Sub subroutine.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : ClearBlack
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Clear the black pixel in the background of "START" and "END" strings.
;------------------------------------------------------------------------------------------------

ClearBlack:                             
    mov  ax, 0                          ; Move 0 Into AX.
    mov  di, 0                          ; Move 0 Into DI.
    black1_y:                           ; black1_y
                                        
    mov  cx, 320                        ; Move 320 Into CX.
    black1_x:                           ; black1_x
                                        
    mov  bl, es:[di]                    ; Move es:[di] Into BL.
                                        
    cmp  bl, 0                          ; Compare the BL register to 0.
    jne  con_blk                        ; Jump if not equal.
                                        
    mov  es:[di], 1                     ; Move 1 Into es:[di].
                                        
    con_blk:                            ; con_blk
    inc  di                             ; Incerment DI.
                                        
    loop black1_x                       ; Redo the loop.
    inc  ax                             ; Incerment AX.
    cmp  ax, 200                        ; Compare the AX register to 200.
    jl   black1_y                       ;
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : MatrixPosition
; Function Inputs		: Player_X, Player_Y - players coordinates.
; Function Outputs		: AX - players pixel (in its cube).                               
; Function Operation	: Calculate the upper_left pixel in the player cube.
;------------------------------------------------------------------------------------------------
                                        
MatrixPosition:                         
    mov  ah, 0                          ; Move 0 Into AH.
    mov  al, ds:Player_Y                ; Move ds:Player_Y Into AL.
    mov  bx, 40                         ; Move 40 Into BX.
    mul  bx                             ; Multiply AX by BX.
    mov  bl, ds:Player_X                ; Move ds:Player_X Into BL.
    add  ax, bx                         ; Add BX to AX.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : CreateMatrix
; Function Inputs		: None.
; Function Outputs		: Maze.                               
; Function Operation	: Fill the maze array with values.
;------------------------------------------------------------------------------------------------

CreateMatrix:                           
    mov  ds:cord_Y, 0                   ; Move 0 Into ds:cord_Y.
    mov  bx, 0                          ; Move 0 Into BX.
    loop_y:                             ; loop_y
        mov  ds:cord_X, 0               ; Move 0 Into ds:cord_X.
        loop_x:                         ; loop_x
            push bx                     ; Push BX register into the stack.
            call SetXY                  ; Call the SetXY subroutine.
            pop  bx                     ; Pop BX register from the stack.
            mov  ax, es:[di]            ; Move es:[di] Into AX.
            cmp  al, 14                 ; Compare the AL register to 14.
            jne  con_loop               ; Jump if not equal.
            mov  ds:Maze[bx], 2         ; Move 2 Into ds:Maze[BX].
            con_loop:                   ; con_loop
                add  ds:cord_X, 1       ; Add 1 to ds:cord_X.
                inc  bx                 ; Incerment BX.
                cmp  ds:cord_X, 40      ; Compare the ds:cord_X register to 40.
        jl loop_x                       ; Jump if Lower.
        add  ds:cord_Y, 1               ; Add 1 to ds:cord_Y.
        cmp  ds:cord_Y, 25              ; Compare the ds:cord_Y register to 25.
    jl loop_y                           ; Jump if Lower.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : CreateGame
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Create the default game.
;------------------------------------------------------------------------------------------------

CreateGame:                             
    call ResetVariables                 ; Call the ResetVariables subroutine.
    call DefineGraphicScreen            ; Call the DefineGraphicScreen subroutine.
    call ColorScreen                    ; Call the ColorScreen subroutine.
    call DefineDefaultScreen            ; Call the DefineDefaultScreen subroutine.
    call CreateMatrix                   ; Call the CreateMatrix subroutine.
    call Print_Start                    ; Call the Print_Start subroutine.
    call Print_End                      ; Call the Print_End subroutine.
    call ClearBlack                     ; Call the ClearBlack subroutine.
    call SetPlayer                      ; Call the SetPlayer subroutine.
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : ColorScreen
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Color the screen according to the maze array values.
;------------------------------------------------------------------------------------------------

ColorScreen:                            
    mov  ds:cord_Y, 0                   ; Move 0 Into ds:cord_Y.
    mov  ds:cord_X, 0                   ; Move 0 Into ds:cord_X.
    mov  cx, 25*40                      ; Move 1000 Into CX.
    color_scr:                          ; color_scr
        push cx                         ; Push CX register into the stack.
        call SetXY                      ; Call the SetXY subroutine.
        call DetermineColor             ; Call the DetermineColor subroutine.
        call ColorCube                  ; Call the ColorCube subroutine.
        pop  cx                         ; Pop CX register from the stack.
        add  ds:cord_Y, 1               ; Add 1 to ds:cord_Y.
        cmp  ds:cord_Y, 25              ; Compare the ds:cord_Y register to 25.
        jne  con_loop1                  ; Jump not equal.
        mov  ds:cord_Y, 0               ; Move 0 Into ds:cord_Y.
        add  ds:cord_X, 1               ; Add 1 to ds:cord_X.
        con_loop1:                      ; con_loop1
    loop color_scr                      ; Redo the loop.
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : SetPlayer
; Function Inputs		: PLayer_X, Player_Y - players coordinates.
; Function Outputs		: None.                               
; Function Operation	: Draw the player in the beggining of the game.
;------------------------------------------------------------------------------------------------

SetPlayer:                              
    mov  al, Player_X                   ; Move Player_X Into AL.
    mov  bl, Player_Y                   ; Move Player_Y Into BL.
    mov  ds:cord_X, al                  ; Move AL Into ds:cord_X.
    mov  ds:cord_Y, bl                  ; Move BL Into ds:cord_Y.
    call SaveCell                       ; Call the SaveCell subroutine.
    call DrawCarRight                   ; Call the DrawCarRight subroutine.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : LoadCell
; Function Inputs		: Cord_X, cord_Y - the position the player was in.
; Function Outputs		: None.                               
; Function Operation	: Load the cube to the screen.
;------------------------------------------------------------------------------------------------

LoadCell:                               
    call SetXY                          ; Call the SetXY subroutine.
    mov  ax, 0                          ; Move 0 Into AX.
    mov  si, 0                          ; Move 0 Into SI.
    loop_row:                           ; loop_row
        mov  cx, 8                      ; Move 8 Into CX.
        loop_col:                       ; loop_col
            mov  dl, ds:Pixel_data[si]  ; Move ds:Pixel_Data[si] Into DL.
            mov  es:[di], dl            ; Move DL Into es:[di].
            add  di, 1                  ; Add 1 to DI.
            add  si, 1                  ; Add 1 to SI.
        loop loop_col                   ; Redo the loop.
        add  ax, 1                      ; Add 1 to AX.
        add  di, 320-8                  ; Add 312 to DI.
        cmp  ax, 8                      ; Compare the AX register to 8.
    jl loop_row                         ; Jump if lower.
mov Pixel_pointer, 0                    ; Move 0 to Pixel_Pointer.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : SaveCell
; Function Inputs		: cord_X, Cord_Y - the position the player is going to move to..
; Function Outputs		: None.                               
; Function Operation	: Save the pixel the player is going to move to.
;------------------------------------------------------------------------------------------------

SaveCell:                               
    call SetXY                          ; Call the SetXY subroutine.
    mov  ax, 0                          ; Move 0 Into AX.
    loop_row1:                          ; loop_row1
        mov  cx, 8                      ; Move 8 Into CX.
        loop_col1:                      ; loop_col1
            mov  dl, es:[di]            ; Move es:[di] Into DL.
            mov  si, Pixel_pointer      ; Move Pixel_pointer Into SI.
            mov  ds:Pixel_data[si], dl  ; Move DL Into ds:Pixel_data[si].
            inc  si                     ; Increment SI.
            mov  Pixel_pointer, si      ; Move SI Into Pixel_pointer.
            add  di, 1                  ; Add 1 to DI.
        loop loop_col1                  ; Redo the loop.
        add  ax, 1                      ; Add 1 to AX.
        add  di, 320-8                  ; Add 312 to DI.
        cmp  ax, 8                      ; Compare AX to 8.
    jl loop_row1                        ; Jump if Lower.
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : ColorCube
; Function Inputs		: DI - first pixel to color.
; Function Outputs		: None.                               
; Function Operation	: Color a cube in the screen (8x8).
;------------------------------------------------------------------------------------------------

ColorCube:                              
    mov  bx, 1                          ; Move 1 Into BX.
    color_row:                          ; color_row
        mov  cx, 8                      ; Move 8 Into CX.
        color_pixel:                    ; color_pixel
            mov  es:[di], al            ; Move AL Into es:[di].
            add  di, 1                  ; Add 1 to DI.
        loop color_pixel                ; Redo the loop.
        add  bx, 1                      ; Add 1 to BX.
        add  di, 312                    ; Add 312 to DI.
        cmp  bx, 8                      ; Compare BX to 8.
    jle  color_row                      ; Jump if lower or equal.
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : DetermineColor
; Function Inputs		: ds:cord_X, ds:cord_Y - position of the player.
; Function Outputs		: AL - the color of the cube.                               
; Function Operation	: Determine the color of the cube you make.
;------------------------------------------------------------------------------------------------

DetermineColor:                         
    mov  ah, 0                          ; Move 0 Into AH.
    mov  al, ds:cord_Y                  ; Move ds:cord_Y Into AL.
    mov  bx, 40                         ; Move 40 Into BX.
    mul  bx                             ; Multiply AX by BX.
    mov  bx, ax                         ; Move AX Into BX.
    mov  ah, 0                          ; Move 0 Into AH.
    mov  al, ds:cord_X                  ; Move ds:cord_X Into AL.
    add  bx, ax                         ; Add AX to BX.
    cmp  bx, 1000                       ; Compare BX to 1000.
    jl   k                              ; Jump if lower.
    mov  al, 6                          ; Move 6 Into AL.
    ret                                 ; Return from the subroutine.
    k:                                  ; k
        mov  al, ds:Maze[bx]            ; Move ds:Maze[bx] Into AL.
        cmp  al, 1                      ; Compare AL to 1.
        jne  con_if                     ; Jump not equal.
        mov  al, 1                      ; Move 1 Into AL.
        ret                             ; Return from the subroutine.
        con_if:                         ; con_if
            cmp  ax, 2                  ; Compare AX to 2.
        jne  con_if2                    ; Jump not equal.
        ;mov  al, 14                    ;
        jmp  end1                       ; Jump to end1.
        con_if2:                        ; con_if2
            ;mov  al, 14                ;
    end1:                               ; end1
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : SetXY
; Function Inputs		: ds:cord_X, ds:cprd_Y  - the player position.
; Function Outputs		: DI - the upper left pixel the player is in.                               
; Function Operation	: Calculate the upper pixel the player is in.
;------------------------------------------------------------------------------------------------

SetXY:                                  
   mov  al, ds:cord_Y                   ; Move ds:cord_Y Into AL.
   mov  bx, 320*8                       ; Move 320*8 Into BX.
   mul  bx                              ; Multiply AX by BX.
   mov  cx, ax                          ; Move AX Into CX.
   mov  ah, 0                           ; Move 0 Into AH.
   mov  al, ds:cord_X                   ; Move ds:cord_X Into AL.
   mov  bx, 8                           ; Move 8 Into BX.
   mul  bx                              ; Multiply AX by BX.
   add  cx, ax                          ; Add AX to CX.
   mov  di, cx                          ; Move CX Into DI.
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : DefineGraphicScreen
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Defining the Graphic screen.
;------------------------------------------------------------------------------------------------
                                        
DefineGraphicScreen:                    
    mov  ah, 0                          ; Move 0 Into AH.
    mov  al, 13h                        ; Move 13h Into AL.
    int  10h                            ; Execute the Interrupt.
    mov  ax, 0a000h                     ; Move 0a000h Into AX.
    mov  es, ax                         ; Move AX Into ES.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : DefineDataSegment
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Defining the Data segment.
;------------------------------------------------------------------------------------------------

DefineDataSegment:                      
    mov  ax, @data                      ; Move Data Segment address into AX register.
    mov  ds, ax                         ; Move AX register into DS register.
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : EndOfProgram
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: End the program.
;------------------------------------------------------------------------------------------------

EndOfProgram:                           
    mov  ax, 3                          ; Move 3 Into AX.
    int  10h                            ; Execute the Interrupt.
    mov  ah, 4ch                        ; Move 4Ch Into AH.
    int  21h                            ; Execute the Interrupt.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : DefineDefaultScreen
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Draws the defualt maze.
;------------------------------------------------------------------------------------------------

DefineDefaultScreen:                    
    mov  Direction, DIR_LTR             ; Move DIR_LTR Into Direction.
                                        
    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 0                  ; Move 0 into Location_Y.
    mov  Length, 40                     ; Move 40 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 12                     ; Move 12 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 19                 ; Move 19 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 27                 ; Move 27 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 10                     ; Move 10 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 3                  ; Move 3 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 8                      ; Move 8 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 19                 ; Move 19 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 12                     ; Move 12 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 5                  ; Move 5 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 21                 ; Move 21 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 11                     ; Move 11 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 7                  ; Move 7 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 21                 ; Move 21 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 31                 ; Move 31 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 11                 ; Move 11 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 23                 ; Move 23 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 7                  ; Move 7 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 10                     ; Move 10 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 25                 ; Move 25 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 11                     ; Move 11 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 5                  ; Move 5 into Location_X.
    mov  Location_Y, 17                 ; Move 17 into Location_Y.
    mov  Length, 16                     ; Move 16 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 25                 ; Move 25 into Location_X.
    mov  Location_Y, 17                 ; Move 17 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 17                 ; Move 17 into Location_Y.
    mov  Length, 11                     ; Move 11 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 11                 ; Move 11 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 11                     ; Move 11 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 8                      ; Move 8 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 3                  ; Move 3 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 11                     ; Move 11 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 1                      ; Move 1 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 24                 ; Move 24 into Location_Y.
    mov  Length, 40                     ; Move 40 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Direction, DIR_UTD             ; Move DIR_UTD Into Direction.        
                                        
    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 22                     ; Move 22 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 3                  ; Move 3 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 15                     ; Move 15 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 5                  ; Move 5 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 12                     ; Move 12 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 7                  ; Move 7 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 9                  ; Move 9 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 12                 ; Move 12 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 15                 ; Move 15 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 11                     ; Move 11 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 17                 ; Move 17 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 13                     ; Move 13 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 13                     ; Move 13 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 21                 ; Move 21 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 8                      ; Move 8 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 25                 ; Move 25 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 31                 ; Move 31 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 3                  ; Move 3 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 33                 ; Move 33 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 35                 ; Move 35 into Location_X.
    mov  Location_Y, 19                 ; Move 19 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 36                 ; Move 36 into Location_X.
    mov  Location_Y, 3                  ; Move 3 into Location_Y.
    mov  Length, 12                     ; Move 12 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 37                 ; Move 37 into Location_X.
    mov  Location_Y, 19                 ; Move 19 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 39                 ; Move 39 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 22                     ; Move 22 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : DrawRow
; Function Inputs		: ds:Location_X - Cube x
;                         ds:Location_Y - Cube Y
;                         Length        - Cube Length
; Function Outputs		: None.                               
; Function Operation	: Draw a row of cube.
;------------------------------------------------------------------------------------------------

DrawRow:                                
    mov  cx, ds:Location_X              ; Move ds:Location_X Into CX.
    mov  ds:cord_X, cl                  ; Move CL Into ds:cord_X.
    mov  cx, ds:Location_Y              ; Move ds:Location_Y Into CX.
    mov  ds:cord_Y, cl                  ; Move CL Into ds:cord_Y.
    mov  cl, Length                     ; Move Length Into CL.
    color_scr1:                         ; color_scr1
        push cx                         ; Push CX register into the stack.
        call SetXY                      ; Call the SetXY subroutine.
        mov  al, 14                     ; Move 14 Into AL.
        call ColorCube                  ; Call the ColorCube subroutine.
        pop  cx                         ; Pop CX register from the stack.
        cmp  Direction, DIR_UTD         ; Compare DIR_UTD to Direction.
        jne  prnt_ltr                   ; Jump not equal.
        prnt_utd:                       ; print_utd
            add  ds:cord_Y, 1           ; Add 1 to ds:cord_Y
            cmp  ds:cord_Y, 25          ; Compare ds:cord_Y to 25.
            jne  con_loop11             ; Jump not equal.
            mov  ds:cord_Y, 0           ; Move 0 Into ds:cord_Y.
            add  ds:cord_X, 1           ; Add 1 to ds_cord_X.
    loop color_scr1                     ; Redo the loop.
    prnt_ltr:                           ; prnt_ltr
        add  ds:cord_X, 1               ; Add 1 to ds:cord_X.
        cmp  ds:cord_X, 80              ; Compare ds:cord_X to 80.
        jne  con_loop11                 ; Jump not equal.
        mov  ds:cord_X, 0               ; Move 0 Into ds:cord_X.
        add  ds:cord_Y,1                ; Add 1 to ds:cord_Y.
        con_loop11:                     ; con_loop11
    loop color_scr1                     ; Redo the loop.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : DefineRandomScreen1
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Draws the random 1 maze.
;------------------------------------------------------------------------------------------------

DefineRandomScreen1:                    
    mov  Direction, DIR_LTR             ; Move DIR_LTR Into Direction.
                                        
    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 0                  ; Move 0 into Location_Y.
    mov  Length, 40                     ; Move 40 into Length.
    call DrawRow                          
   
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          

    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                          
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
   
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 36                 ; Move 36 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 1                  ; Move 1 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                          
    
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                          
    
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 13                     ; Move 13 into Length.
    call DrawRow                          
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                          
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                          
    
    mov  Location_X, 1                  ; Move 1 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                          
    
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 1                  ; Move 1 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                          
    
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                          
    
    mov  Location_X, 1                  ; Move 1 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                          
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 36                 ; Move 36 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                          
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                          
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                          
    
    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 24                 ; Move 24 into Location_Y.
    mov  Length, 40                     ; Move 40 into Length.
    call DrawRow                          
 
    mov  Direction, DIR_UTD             ; Move DIR_UTD Into Direction.        
                                        
    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 22                     ; Move 22 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                                                                
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 12                 ; Move 12 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 12                 ; Move 12 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 13                     ; Move 13 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
 
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 10                     ; Move 10 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 36                 ; Move 36 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 36                 ; Move 36 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 13                     ; Move 13 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
 
ret                                     ; Return from the subroutine.
                                       
;------------------------------------------------------------------------------------------------
; Function Name		    : DefineRandomScreen1
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Draws the random 2 maze.
;------------------------------------------------------------------------------------------------

DefineRandomScreen2:                    
    mov  Direction, DIR_LTR             ; Move DIR_LTR Into Direction.

    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 0                  ; Move 0 into Location_Y.
    mov  Length, 40                     ; Move 40 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                         
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 30 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 28                 ; Move 20 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 1                  ; Move 1 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 1                  ; Move 1 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 36                 ; Move 36 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 12                 ; Move 12 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 36                 ; Move 36 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 24                 ; Move 24 into Location_Y.
    mov  Length, 40                     ; Move 40 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Direction, DIR_UTD             ; Move DIR_UTD Into Direction.        
    
    mov  Location_X, 0                  ; Move 0 into Location_X.
    mov  Location_Y, 0                  ; Move 0 into Location_Y.
    mov  Length, 24                     ; Move 24 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
    mov  Location_X, 2                  ; Move 2 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 4                  ; Move 4 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 0                  ; Move 0 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 6                  ; Move 6 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 8                  ; Move 8 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 0                  ; Move 0 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 10                 ; Move 10 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 12                 ; Move 12 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 12                 ; Move 12 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 12                 ; Move 12 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 0                  ; Move 0 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 14                 ; Move 14 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 16                 ; Move 16 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 18                 ; Move 18 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 4                      ; Move 4 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 20                 ; Move 20 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 22                 ; Move 22 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 7                      ; Move 7 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 24                 ; Move 24 into Location_X.
    mov  Location_Y, 20                 ; Move 20 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 4                  ; Move 4 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 16                 ; Move 16 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 26                 ; Move 26 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 28                 ; Move 28 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 30                 ; Move 30 into Location_X.
    mov  Location_Y, 18                 ; Move 18 into Location_Y.
    mov  Length, 6                      ; Move 6 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 9                      ; Move 9 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 32                 ; Move 32 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 1                  ; Move 1 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 10                 ; Move 10 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 34                 ; Move 34 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 36                 ; Move 36 into Location_X.
    mov  Location_Y, 6                  ; Move 6 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 36                  ; Move 36 into Location_X.
    mov  Location_Y, 12                 ; Move 12 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 36                 ; Move 36 into Location_X.
    mov  Location_Y, 22                 ; Move 22 into Location_Y.
    mov  Length, 2                      ; Move 2 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 2                  ; Move 2 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 8                  ; Move 8 into Location_Y.
    mov  Length, 3                      ; Move 3 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
    
    mov  Location_X, 38                 ; Move 38 into Location_X.
    mov  Location_Y, 14                 ; Move 14 into Location_Y.
    mov  Length, 5                      ; Move 5 into Length.
    call DrawRow                        ; Call the DrawRow subroutine.
                                        
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : DrawCarUp
; Function Inputs		: cord_X, cord_Y - the player position.
; Function Outputs		: None.                               
; Function Operation	: Draw the player in the Up position.
;------------------------------------------------------------------------------------------------

DrawCarUp:                             
    call SetXY                          ; Call the SetXY subroutine.
                                        
    mov  es:[di+3+320*1], 4             ; Color the player pixel.
    mov  es:[di+4+320*1], 4             ; Color the player pixel.
                                        
    mov  es:[di+2+320*2], 4             ; Color the player pixel.
    mov  es:[di+3+320*2], 4             ; Color the player pixel.
    mov  es:[di+4+320*2], 4             ; Color the player pixel.
    mov  es:[di+5+320*2], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*3], 4             ; Color the player pixel.
    mov  es:[di+2+320*3], 4             ; Color the player pixel.
    mov  es:[di+3+320*3], 4             ; Color the player pixel.
    mov  es:[di+4+320*3], 4             ; Color the player pixel.
    mov  es:[di+5+320*3], 4             ; Color the player pixel.
    mov  es:[di+6+320*3], 4             ; Color the player pixel.
                                        
    mov  es:[di+3+320*4], 4             ; Color the player pixel.
    mov  es:[di+4+320*4], 4             ; Color the player pixel.
                                        
    mov  es:[di+3+320*5], 4             ; Color the player pixel.
    mov  es:[di+4+320*5], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*6], 4             ; Color the player pixel.
    mov  es:[di+2+320*6], 4             ; Color the player pixel.
    mov  es:[di+3+320*6], 4             ; Color the player pixel.
    mov  es:[di+4+320*6], 4             ; Color the player pixel.
    mov  es:[di+5+320*6], 4             ; Color the player pixel.
    mov  es:[di+6+320*6], 4             ; Color the player pixel.
                                        
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : DrawCarRight
; Function Inputs		: cord_X, cord_Y - the player position.
; Function Outputs		: None.                               
; Function Operation	: Draws the player in the Right position.
;------------------------------------------------------------------------------------------------

DrawCarRight:                           
    call SetXY                          ; Call the SetXY subroutine.
                                        
    mov  es:[di+1+320*1], 4             ; Color the player pixel.
    mov  es:[di+4+320*1], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*2], 4             ; Color the player pixel.
    mov  es:[di+4+320*2], 4             ; Color the player pixel.
    mov  es:[di+5+320*2], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*3], 4             ; Color the player pixel.
    mov  es:[di+2+320*3], 4             ; Color the player pixel.
    mov  es:[di+3+320*3], 4             ; Color the player pixel.
    mov  es:[di+4+320*3], 4             ; Color the player pixel.
    mov  es:[di+5+320*3], 4             ; Color the player pixel.
    mov  es:[di+6+320*3], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*4], 4             ; Color the player pixel.
    mov  es:[di+2+320*4], 4             ; Color the player pixel.
    mov  es:[di+3+320*4], 4             ; Color the player pixel.
    mov  es:[di+4+320*4], 4             ; Color the player pixel.
    mov  es:[di+5+320*4], 4             ; Color the player pixel.
    mov  es:[di+6+320*4], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*5], 4             ; Color the player pixel.
    mov  es:[di+4+320*5], 4             ; Color the player pixel.
    mov  es:[di+5+320*5], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*6], 4             ; Color the player pixel.
    mov  es:[di+4+320*6], 4             ; Color the player pixel.
                                        
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : DrawCarLeft
; Function Inputs		: cord_X, cord_Y - the player position.
; Function Outputs		: None.                               
; Function Operation	: Draws the player in the Left position.
;------------------------------------------------------------------------------------------------

DrawCarLeft:                           
    call SetXY                          ; Call the SetXY subroutine.
                                        
    mov  es:[di+3+320*1], 4             ; Color the player pixel.
    mov  es:[di+6+320*1], 4             ; Color the player pixel.
                                        
    mov  es:[di+2+320*2], 4             ; Color the player pixel.
    mov  es:[di+3+320*2], 4             ; Color the player pixel.
    mov  es:[di+6+320*2], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*3], 4             ; Color the player pixel.
    mov  es:[di+2+320*3], 4             ; Color the player pixel.
    mov  es:[di+3+320*3], 4             ; Color the player pixel.
    mov  es:[di+4+320*3], 4             ; Color the player pixel.
    mov  es:[di+5+320*3], 4             ; Color the player pixel.
    mov  es:[di+6+320*3], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*4], 4             ; Color the player pixel.
    mov  es:[di+2+320*4], 4             ; Color the player pixel.
    mov  es:[di+3+320*4], 4             ; Color the player pixel.
    mov  es:[di+4+320*4], 4             ; Color the player pixel.
    mov  es:[di+5+320*4], 4             ; Color the player pixel.
    mov  es:[di+6+320*4], 4             ; Color the player pixel.
                                        
    mov  es:[di+2+320*5], 4             ; Color the player pixel.
    mov  es:[di+3+320*5], 4             ; Color the player pixel.
    mov  es:[di+6+320*5], 4             ; Color the player pixel.
                                        
    mov  es:[di+3+320*6], 4             ; Color the player pixel.
    mov  es:[di+6+320*6], 4             ; Color the player pixel.
                                        
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : DrawCarDown
; Function Inputs		: cord_X, cord_Y - the player position.
; Function Outputs		: None.                               
; Function Operation	: Draws the player in the Down position.
;------------------------------------------------------------------------------------------------

DrawCarDown:                            
    call SetXY                          ; Call the SetXY subroutine.
                                        
    mov  es:[di+3+320*6], 4             ; Color the player pixel.
    mov  es:[di+4+320*6], 4             ; Color the player pixel.
                                        
    mov  es:[di+2+320*5], 4             ; Color the player pixel.
    mov  es:[di+3+320*5], 4             ; Color the player pixel.
    mov  es:[di+4+320*5], 4             ; Color the player pixel.
    mov  es:[di+5+320*5], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*4], 4             ; Color the player pixel.
    mov  es:[di+2+320*4], 4             ; Color the player pixel.
    mov  es:[di+3+320*4], 4             ; Color the player pixel.
    mov  es:[di+4+320*4], 4             ; Color the player pixel.
    mov  es:[di+5+320*4], 4             ; Color the player pixel.
    mov  es:[di+6+320*4], 4             ; Color the player pixel.
                                        
    mov  es:[di+3+320*3], 4             ; Color the player pixel.
    mov  es:[di+4+320*3], 4             ; Color the player pixel.
                                        
    mov  es:[di+3+320*2], 4             ; Color the player pixel.
    mov  es:[di+4+320*2], 4             ; Color the player pixel.
                                        
    mov  es:[di+1+320*1], 4             ; Color the player pixel.
    mov  es:[di+2+320*1], 4             ; Color the player pixel.
    mov  es:[di+3+320*1], 4             ; Color the player pixel.
    mov  es:[di+4+320*1], 4             ; Color the player pixel.
    mov  es:[di+5+320*1], 4             ; Color the player pixel.
    mov  es:[di+6+320*1], 4             ; Color the player pixel.
                                        
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : Print_Start
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Print the start stirng.
;------------------------------------------------------------------------------------------------

Print_Start:                            
    mov  si, offset start_cap           ; Move address of start_cap Into SI.
    mov  dx, 0100h                      ; Move 100h Into dx.
    print_char:                         ; print_char
        mov  ah, 2                      ; Move 2 Into AH.
        mov  bh, 0                      ; Move 0 Into BH.
        int  10h                        ; Execute the interrupt.
        mov  ah, 09h                    ; Move 9h Into AH.
        mov  bh, 00h                    ; Move 0 Into BH.
        mov  bl, 1Fh                    ; Move 1Fh Into BL.
        mov  al, [si]                   ; Move [si] Into AL.
        mov  cx, 1                      ; Move 1 Into CX.
        add  si, 1                      ; Add 1 to SI.
        int  10h                        ; Execute the Interrupt.
        add  dx, 1                      ; Add 1 to DX.
        mov  al, [si]                   ; Move [si] Into AL.
        cmp  al, '$'                    ; Compare AL to '$' Char.
    jne  print_char                     ; Jump not equal.
ret                                     ; Return from the subroutine.
                                        
;------------------------------------------------------------------------------------------------
; Function Name		    : Print_End
; Function Inputs		: None.
; Function Outputs		: None.                               
; Function Operation	: Print the end string.
;------------------------------------------------------------------------------------------------

Print_End:                              
    mov  si, offset end_cap             ; Move address of end_cap Into SI.
    mov  dx, 174Dh                      ; Move 174Dh Into DX.
    print_char1:                        ; print_char1
        mov  ah, 2                      ; Move 2 Into AH.
        mov  bh, 0                      ; Move 0 Into BH.
        int  10h                        ; Execute the Interrupt.
        mov  ah, 09h                    ; Move 9h Into AH.
        mov  bh, 00h                    ; Move 0 Into BH.
        mov  bl, 1Fh                    ; Move 1Fh Into BL.
        mov  al, [si]                   ; Move [SI] Into AL.
        mov  cx, 1                      ; Move 1 Into CX.
        add  si, 1                      ; Add 1 to SI.
        int  10h                        ; Execute the Interrupt.
        add  dx, 1                      ; Add 1 to DI.
        mov  al, [si]                   ; Move [si] Into AL.
        cmp  al, '$'                    ; Compare AL to '$' Char.
    jne  print_char1                    ; Jump not equal.
ret                                     ; Return from the subroutine.

;------------------------------------------------------------------------------------------------
; Function Name		    : Wait_Sub
; Function Inputs		: BX Register
; Function Outputs		: None.                               
; Function Operation	: Wait the progress of the program.
;------------------------------------------------------------------------------------------------

Wait_Sub:                               ; Delay Subroutine.
    loop Wait_Sub                       ; Loop until the CX register reach zero.
    dec  bx                             ; Decrement BX by 1.
    jnz  Wait_Sub                       ; If not equal 0 jump back to the Delay Subroutine.
ret                                     ; Return from the subroutine.
                                        
end                                     ; End of Code.
