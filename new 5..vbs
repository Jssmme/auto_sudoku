Dim z,z0,a
dim x10 = 37
//dim y10 = 478
dim y10 = 406
z = 111
Dim x(),y()
For i = 0 To 8
    x(i) = x10 + z * i + 4 * (i \ 3)
    y(i) = y10 + z * i + 4 * (i \ 3)
Next
Dim c0,c1,c2,c3,c4
Dim board(9,9)
Dim puz2(9,9)

//Delay 1000
//Tap 511, 2000
//For i = 0 To 7
//	TracePrint i*0.5
//	Delay 500
//Next
Rem ks
a = GetPixelColor (481, 749)
If a <> "E8DFD9" Then 
	TracePrint "worng start"
	EndScript
	Else 
	Tap 484, 1546
	Delay 1500
End If
dus ()
TracePrint "solving"
solver (board)
For i = 0 To 8
    a = null
    For u = 0 To 8
        a = a & board(i,u)
    Next
    TracePrint a
Next

For i = 0 To 8
    For u = 0 To 8
        If board(i, u) = 0 Then 
            TracePrint "??¡ä¨ª¨¢?"
            EndScript 
        End If
    Next
Next

For i = 0 To 8
    For u = 0 To 8
        If puz2(i, u) = 0 and board(i,u) <> 0 Then 
            tis (i, u)
        End If
    Next
Next
TracePrint "finish!"

//For i = 0 To 7
//	TracePrint i*0.5
	Delay 2500
//Next
//
Tap 484, 2072
Delay 2000
Goto ks
//?¨¢¨ºy??¨ºy¡Á¨¦
Function dus()
    KeepCapture 
    For i = 0 To 8
        For u = 0 To 8
            board(i, u) = shu(x(u), y(i))
            puz2(i, u) = board(i, u)
            z0 = z0&board(i,u)
        Next 
        TracePrint z0
        z0 = null
    Next
    ReleaseCapture 
End Function

Function valid(board, i, j, c)
    For k = 0 To 8
        If board(i,k) = c or board(k,j) = c Then
            valid = False
            Exit Function
        End If
    Next
    Dim bi = Int(i / 3) * 3
    Dim bj = Int(j / 3) * 3
    For k=0 to 2
        if(board(bi+k,bj) = c or board(bi+k,bj+1) = c or board(bi+k,bj+2) = c) Then
            valid = False
            Exit Function
        End If
    Next
    valid = True
    Exit Function
End Function

Function solver(board)
    For i=0 to 8
        For j=0 to 8
            If board(i,j)=0 Then
                For k = 1 To 9
                    If valid(board, i, j, k) Then 
                        board(i, j) = k
                        If solver(board) Then 
                            solver = True
                            Exit Function
                        End If
                        board(i,j) = 0
                    End If
                Next
                solver = False
                Exit Function
            End If
        Next
    Next
    solver = True
    Exit Function
End Function

Function shu(x, y)
    c1 = GetPixelColor(x - 1, y - 1)
    c2 = GetPixelColor(x - 1, y)
    c3 = GetPixelColor(x, y - 1)
    c4 = GetPixelColor(x, y)
    shu = 0
    If c1 <> "707070" Then 
        TracePrint "error base point"
        EndScript
    End If
    If c2 <> "707070" Then 
        TracePrint "error base point"
        EndScript
    End If
    If c3 <> "707070" Then 
        TracePrint "error base point"
        EndScript
    End If
    If c4 = "707070" Then 
        TracePrint "error base point"
        EndScript
    End If
    c0 = GetPixelColor(x + 74, y + 49)
    If c0 = "2C2C2C" Then 
        shu = 9
        Exit Function	
    End If
    c0 = GetPixelColor(x + 33, y + 79)
    If c0 = "2C2C2C" Then 
        shu = 8
        Exit Function
    End If
    c0 = GetPixelColor(x + 33, y + 19)
    If c0 = "2C2C2C" Then 
        shu = 7
        Exit Function
    End If
    c0 = GetPixelColor(x + 34, y + 58)
    If c0 = "2C2C2C" Then 
        shu = 6
        Exit Function
    End If
    c0 = GetPixelColor(x + 45, y + 36)
    If c0 = "2C2C2C" Then 
        shu = 5
        Exit Function
    End If
    c0 = GetPixelColor(x + 49, y + 39)
    If c0 = "2C2C2C" Then 
        shu = 4
        Exit Function
    End If
    c0 = GetPixelColor(x + 33, y + 35)
    If c0 = "2C2C2C" Then 
        shu = 3
        Exit Function
    End If
    c0 = GetPixelColor(x + 77, y + 90)
    If c0 = "2C2C2C" Then 
        shu = 2
        Exit Function
    End If
    c0 = GetPixelColor(x + 57, y + 78)
    If c0 = "2C2C2C" Then 
        shu = 1
        Exit Function
    End If
End Function


Function tis(i,u)
    Delay 100
//    TracePrint i&u
    Tap x(u) + 40, y(i) + 40
    Delay 100
    Tap board(i, u) * 115 - 35, 1900
    Delay 50
End Function

