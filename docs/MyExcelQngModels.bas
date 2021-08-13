Attribute VB_Name = "Module4"

'Declare Function erlang_c Lib "C:\WORK\TOOLS\QDLL\MGQUEUES.DLL" (ByVal blLoad As Double, ByVal intN As Integer) As Double
'Declare Function erlang_c Lib "I:\data\systems\man_eng\computer\TOOLS\QDLL\MGQUEUES.DLL" (ByVal blLoad As Double, ByVal intN As Integer) As Double

Function PoissonInv(dblMean As Double, dblProb As Double) As Double

'Purpose:   Returns the cumulative inverse of the Poisson distribution.
'           Useful for capacity planning (e.g. bedsizing)

'Arguments: dblMean     Mean of the Poisson distribution
'           dblProb     Probability for which the inverse is to be calculated

'Returns:   Double     Inverse Poisson value

'Author:    Mark Isken
'Created:   7/23/96
'Modified:

Dim dblCount As Double
Dim blnFoundCum As Boolean

    If dblMean < 0# Then
        'MsgBox "Mean must be greater than zero.", , "Inverse Poisson"
        PoissonInv = CVErr(xlErrValue)
        Exit Function
    End If
    If dblMean = 0# Then
        'MsgBox "Mean must be greater than zero.", , "Inverse Poisson"
        PoissonInv = 0
        Exit Function
    End If
    If dblProb <= 0 Or dblProb > 1# Then
        'MsgBox "Invalid probability.", , "Inverse Poisson"
        PoissonInv = CVErr(xlErrValue)
        Exit Function
    End If
    
    If dblMean > 50 Then
        PoissonInv = Application.NormInv(dblProb, dblMean, Sqr(dblMean))
    Else
        blnFoundCum = False
        dblCount = 0
        While Not blnFoundCum
            If Application.Poisson(dblCount, dblMean, True) >= dblProb Then
                blnFoundCum = True
                PoissonInv = dblCount
            Else
                dblCount = dblCount + 1#
            End If
        Wend
            
    End If
    
End Function

Function ErlangLossInv(dblMean As Double, dblProb As Double) As Double
'Purpose:   Returns the cumulative inverse of the Erlang loss distribution.
'           Useful for capacity planning (e.g. bedsizing)

'Arguments: dblMean     Mean of the Poisson distribution
'           dblProb     Probability for which the inverse is to be calculated

'Returns:   Double     Inverse Poisson value

'Author:    Mark Isken
'Created:   09/09/96
'Modified:

Dim dblCount As Double
Dim blnFoundCum As Boolean

    If dblMean < 0# Then
        'MsgBox "Mean must be greater than zero.", , "Inverse Erlang Loss"
        ErlangLossInv = CVErr(xlErrValue)
        Exit Function
    End If
    If dblMean = 0# Then
        'MsgBox "Mean must be greater than zero.", , "Inverse Erlang Loss"
        ErlangLossInv = 0
        Exit Function
    End If
    If dblProb <= 0 Or dblProb > 1# Then
        'MsgBox "Invalid probability.", , "Inverse Erlang Loss"
        ErlangLossInv = CVErr(xlErrValue)
        Exit Function
    End If
    

    blnFoundCum = False
    dblCount = 1
    While Not blnFoundCum
        If ErlangB(dblMean, Int(dblCount)) <= dblProb Then
            blnFoundCum = True
            ErlangLossInv = dblCount
        Else
            dblCount = dblCount + 1#
        End If
    Wend
End Function

Function ErlangLoss(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer) As Double

'Purpose:   Returns the probability of loss in M/G/c/c system.
'           For large c, the normal approximation to the poisson is used.

'Arguments: dblArrivalRate     Mean of the Poisson arrival process
'           dblAvgServiceTime  Average service time in same units as arrival rate
'           intNumServers      Number of servers (c)


'Returns:   Double             Erlang loss probability

'Author:    Mark Isken
'Created:   7/30/96
'Modified:

Dim dblPDF As Double, dblCDF As Double, dblPDFUpper As Double, dblPDFLower As Double
Dim dblCDFUpper As Double, dblCDFLower As Double


    If dblArrivalRate < 0# Or dblAvgServiceTime < 0 Then
        'MsgBox "Arrival rate & average service time must be >= 0.", , "Erlang Loss"
        ErlangLoss = CVErr(xlErrValue)
        Exit Function
    End If
    If intNumServers <= 0 Then
        'MsgBox "Number of servers must be > 0.", , "Erlang Loss"
        ErlangLoss = CVErr(xlErrValue)
        Exit Function
    End If

    
    If (dblArrivalRate * dblAvgServiceTime) <= 100 Then
        dblPDF = Application.Poisson(intNumServers, (dblArrivalRate * dblAvgServiceTime), False)
        dblCDF = Application.Poisson(intNumServers, (dblArrivalRate * dblAvgServiceTime), True)
    Else
        ErlangLoss = ErlangB(dblArrivalRate * dblAvgServiceTime, intNumServers)
        Exit Function
    End If
    
    If dblCDF < 0.001 Then
        ErlangLoss = 9999
    Else
        ErlangLoss = dblPDF / dblCDF
    End If
    
    Exit Function
        
End Function

Function ErlangProb_n(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer, intNumCustomers As Integer) As Double

'Purpose:   Returns the probability of n customers in M/G/c/c system.
'           For large c, the normal approximation to the poisson is used.

'Arguments: dblArrivalRate     Mean of the Poisson arrival process
'           dblAvgServiceTime  Average service time in same units as arrival rate
'           intNumServers      Number of servers (c)
'           intNumCustomers    Number of customers (n)

'Returns:   Double             Occupancy of n probability

'Author:    Mark Isken
'Created:   9/5/96
'Modified:

Dim dblPDFUpper, dblPDFLower, dblPDF As Double, dblCDF As Double


    If dblArrivalRate < 0# Or dblAvgServiceTime < 0 Then
        'MsgBox "Arrival rate & average service time must be >= 0.", , "Erlang Prob(n)"
        ErlangProb_n = CVErr(xlErrValue)
        Exit Function
    End If
    If intNumServers <= 0 Then
        'MsgBox "Number of servers must be > 0.", , "Erlang Prob(n)"
        ErlangProb_n = CVErr(xlErrValue)
        Exit Function
    End If
    If n > intNumServers Then
        'MsgBox "Number of customers must be <= Number of servers", , "Erlang Loss"
        ErlangProb_n = CVErr(xlErrValue)
        Exit Function
    End If
    
    If (dblArrivalRate * dblAvgServiceTime) <= 40 Then
        dblPDF = Application.Poisson(intNumCustomers, (dblArrivalRate * dblAvgServiceTime), False)
        dblCDF = Application.Poisson(intNumServers, (dblArrivalRate * dblAvgServiceTime), True)
    Else
        dblPDFUpper = Application.NormDist((intNumCustomers + 0.5 - (dblArrivalRate * dblAvgServiceTime)) / Sqr(dblArrivalRate * dblAvgServiceTime), 0, 1, True)
        dblPDFLower = Application.NormDist((intNumCustomers - 0.5 - (dblArrivalRate * dblAvgServiceTime)) / Sqr(dblArrivalRate * dblAvgServiceTime), 0, 1, True)
        dblPDF = dblPDFUpper - dblPDFLower
        
        dblCDFUpper = Application.NormDist((intNumServers + 0.5 - (dblArrivalRate * dblAvgServiceTime)) / Sqr(dblArrivalRate * dblAvgServiceTime), 0, 1, True)
        dblCDF = dblCDFUpper
    End If
    
    ErlangProb_n = dblPDF / dblCDF
    Exit Function
End Function

Function ErlangB(dblLoad As Double, intNumServers As Integer) As Double

'Purpose:   Returns the probability of loss in M/G/c/c system.
'           Used recursive approach outline in "Traffic Flow in Switching Systems"
'           by Gerard Hebuterne (translated by David Oliver)
'           Artech House, Boston, MA, 1987

'           For large values of dblLoad and intNumServers an exponential
'           approximation and Stirling's approximation are used to directly
'           evaluate the Erlang B formula.

'Arguments: dblLoad            Mean of the Poisson occupancy process
'           intNumServers      Number of servers (c)


'Returns:   Double             Erlang loss probability

'Author:    Mark Isken
'Created:   7/30/96
'Modified:

Dim dblProb As Double, dblProbInverse As Double

Const PI = 3.14159265

    If dblLoad < 0 Then
        'MsgBox "Load must be >= 0.", , "Erlang B"
        ErlangB = CVErr(xlErrValue)
        Exit Function
    End If
    If intNumServers < 0 Then
        'MsgBox "Number of servers must be >= 0.", , "Erlang B"
        ErlangB = CVErr(xlErrValue)
        Exit Function
    End If
    
    
    If intNumServers = 0 Then
        ErlangB = 1#
        Exit Function
    End If
    
    If intNumServers <= 250 Then
        dblProbInverse = 1 + (intNumServers / dblLoad) * (1 / ErlangB(dblLoad, intNumServers - 1))
        ErlangB = 1 / dblProbInverse
        Exit Function
    Else
        dblProbInverse = Sqr(2 * PI * intNumServers)
        dblProb = Exp(intNumServers - dblLoad + intNumServers * Log(dblLoad / intNumServers))
        ErlangB = dblProb / dblProbInverse
        Exit Function
    End If
    
    
    Exit Function
End Function

Function ErlangBProb_n(dblLoad As Double, intNumServers As Integer, intNumCustomers As Integer)

'Purpose:   Returns the probability of occupancy n in M/G/c/c system.
'           Used recursive approach outline in "Traffic Flow in Switching Systems"
'           by Gerard Hebuterne (translated by David Oliver)
'           Artech House, Boston, MA, 1987

'           For large values of dblLoad and intNumServers an exponential
'           approximation and Stirling's approximation are used to directly
'           evaluate the Erlang B formula.

'Arguments: dblLoad            Mean of the Poisson occupancy process
'           intNumServers      Number of servers (c)


'Returns:   Double             Erlang occupancy probability

'Author:    Mark Isken
'Created:   9/5/96
'Modified:

Dim dblProb As Double, dblDenom As Double, dblNumer As Double

Const PI = 3.14159265

    If dblLoad < 0 Then
        'MsgBox "Load must be >= 0.", , "Erlang B Prob(n)"
        ErlangBProb_n = CVErr(xlErrValue)
        Exit Function
    End If
    If intNumServers < 0 Or intNumCustomers < 0 Then
        'MsgBox "Number of servers & customers must be >= 0.", , "Erlang B Prob(n)"
        ErlangBProb_n = CVErr(xlErrValue)
        Exit Function
    End If
    
    
    If intNumServers = intNumCustomers Then
        ErlangBProb_n = ErlangB(dblLoad, intNumServers)
        Exit Function
    End If
    
    If intNumServers <= 50 Then
        dblDenom = 0#
        For intCount = 0 To intNumServers
            dblDenom = dblDenom + dblLoad ^ intCount / Application.Fact(intCount)
        Next intCount
        dblNumer = dblLoad ^ intNumCustomers / Application.Fact(intNumCustomers)
        ErlangBProb_n = dblNumer / dblDenom
        Exit Function
    Else
        dblDenom = Exp(dblLoad)
        dblNumer = dblLoad ^ intNumCustomers / ((intNumCustomers ^ (intNumCustomers + 0.5)) * Exp(-intNumCustomers) * Sqr(2 * PI))
        ErlangBProb_n = dblNumer / dblDenom
        Exit Function
    End If
    Exit Function
End Function

Function MMC_CProbWait(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer)

    If dblArrivalRate * dblAvgServiceTime > intNumServers Or dblArrivalRate * dblAvgServiceTime < 0 Then
        MMC_CProbWait = CVErr(xlErrValue)
        Exit Function
    End If
    
    
    'dblProbOfWait = erlang_c(dblArrivalRate * dblAvgServiceTime, intNumServers)
    MMC_CProbWait = dblProbOfWait
    Exit Function
 
End Function


Function MMC_AvgWait(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer)

Dim dblAvgWait As Double

    If dblArrivalRate * dblAvgServiceTime > intNumServers Or dblArrivalRate * dblAvgServiceTime < 0 Then
        MMC_AvgWait = CVErr(xlErrValue)
        Exit Function
    End If

    dblAvgWait = MMC_ProbWait(dblArrivalRate, dblAvgServiceTime, intNumServers) / ((1# / dblAvgServiceTime) * (intNumServers - dblArrivalRate * dblAvgServiceTime))
    MMC_AvgWait = dblAvgWait

End Function
Function MMC_ProbWait(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer)

'Purpose:   Returns the probability of delay in M/M/c system.
'           Used approach outline in "Traffic Flow in Switching Systems"
'           by Gerard Hebuterne (translated by David Oliver)
'           Artech House, Boston, MA, 1987

'Arguments: dblArrivalRate     Mean of the Poisson arrival process
'           dblAvgServiceTime  Mean of exponential service time distribution
'           intNumServers      Number of servers (c)


'Returns:   Double             MMC delay probability

'Author:    Mark Isken
'Created:   03/04/1997
'Modified:
   
Dim dblRho As Double, dblErlangB As Double


    dblRho = dblArrivalRate * dblAvgServiceTime / intNumServers
    
    If dblArrivalRate * dblAvgServiceTime > intNumServers Or dblArrivalRate * dblAvgServiceTime < 0 Then
        MMC_ProbWait = CVErr(xlErrValue)
        Exit Function
    End If
    
    dblErlangB = ErlangB(dblArrivalRate * dblAvgServiceTime, intNumServers)
    dblProbOfWait = 1 / (dblRho + (1 - dblRho) * (1 / dblErlangB))
    MMC_ProbWait = dblProbOfWait
    Exit Function
    
End Function
Function MMC_ProbWaitInv(dblArrivalRate As Double, dblAvgServiceTime As Double, dblProb As Double) As Double
'Purpose:   Returns the cumulative inverse of the Erlang delay distribution.
'           Useful for capacity planning (e.g. bedsizing)

'Arguments: dblArrivalRate
'           dblAvgServiceTime
'           dblProb     Probability for which the inverse is to be calculated

'Returns:   Double     Inverse Poisson value

'Author:    Mark Isken
'Created:   01/19/97
'Modified:

Dim intCount As Integer, dblMean As Double
Dim blnFoundCum As Boolean

    dblMean = dblArrivalRate * dblAvgServiceTime
    If dblMean < 0# Then
        'MsgBox "Mean must be greater than zero.", , "Inverse Erlang Delay"
        MMC_ProbWaitInv = CVErr(xlErrValue)
        Exit Function
    End If
    If dblMean = 0# Then
        'MsgBox "Mean must be greater than zero.", , "Inverse Erlang Delay"
        MMC_ProbWaitInv = 0
        Exit Function
    End If
    If dblProb <= 0 Or dblProb > 1# Then
        'MsgBox "Invalid probability.", , "Inverse Erlang Delay"
        MMC_ProbWaitInv = CVErr(xlErrValue)
        Exit Function
    End If
    

    blnFoundCum = False
    If dblArrivalRate * dblAvgServiceTime - Int(dblArrivalRate * dblAvgServiceTime) = 0 Then
        intCount = Int(dblArrivalRate * dblAvgServiceTime)
    Else
        intCount = Int(dblArrivalRate * dblAvgServiceTime) + 1
    End If
    
    While Not blnFoundCum
        If MMC_ProbWait(dblArrivalRate, dblAvgServiceTime, intCount) <= dblProb Then
            blnFoundCum = True
            MMC_ProbWaitInv = intCount
        Else
            intCount = intCount + 1#
        End If
    Wend
End Function

Sub FunctionTester()
Dim i As Integer, lambda As Double, b As Double, b2 As Double
Dim c As Integer, Prob As Double, y As Double, Lq As Double

  lambda = 40
  b = 1
  b2 = Sqr(0.5)
  c = 50
  Prob = 0.95
  
  'x = ErlangLoss(lambda, b, c)
  'MsgBox "Erlang Loss " & Str(x)
  x = MMC_AvgWait(lambda, b, c)
  y = MDC_AvgWait_CosmetatosApprox(lambda, b, c)
  'z = 'MGC_AvgWait_Approx(lambda, b, b2, c)
  Lq = Z * lambda
  
  MsgBox "MMC_AvgWait " & Str(x)
  MsgBox "MDC_AvgWait_CosmetatosApprox " & Str(y)
  MsgBox "MGC_AvgWait_Approx " & Str(Z)
  MsgBox "MGC_AvgQLength_Approx " & Str(Lq)
  'z = ErlangLossInv(lambda * b, Prob)
  'MsgBox "Needed at " & Prob & " - " & z
End Sub

Sub SumTo1()
Dim i As Integer

  x = 0
  For i = 0 To 158
    x = x + ErlangProb_n(155, 1, 158, i)
  Next i
  MsgBox x
End Sub

Public Function MMC_QWaitCDF(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer, dblTargetTime As Double) As Double

'Purpose:   Returns the probability that Wq is <= t in M/M/c.


'Arguments: dblArrivalRate     Mean of the Poisson arrival process
'           dblAvgServiceTime  Mean of exponential service time distribution
'           intNumServers      Number of servers (c)
'           dblTargetTime      t in P[Wq<=t]


'Returns:   Double             P[Wq<=t]

'Author:    Mark Isken
'Created:   06/20/1997
'Modified:
   
Dim dblRho As Double, dblResult As Double
Dim dblNumerator As Double, dblDenominator As Double, dblPZero As Double, dblProbWait As Double


    dblRho = dblArrivalRate * dblAvgServiceTime / intNumServers
    
    If dblArrivalRate * dblAvgServiceTime > intNumServers Or dblArrivalRate * dblAvgServiceTime < 0 Or dblTargetTime < 0 Then
        MMC_QWaitCDF = CVErr(xlErrValue)
        Exit Function
    End If
    
    dblNumerator = ((dblArrivalRate * dblAvgServiceTime) ^ intNumServers) * (1# - Exp(-(intNumServers / dblAvgServiceTime - dblArrivalRate) * dblTargetTime))
    dblDenominator = Application.Fact(intNumServers - 1) * (intNumServers - dblArrivalRate * dblAvgServiceTime)
    
    dblResult = (dblNumerator / dblDenominator) * MMC_PZero(dblArrivalRate, dblAvgServiceTime, intNumServers) + 1 - MMC_ProbWait(dblArrivalRate, dblAvgServiceTime, intNumServers)
    MMC_QWaitCDF = dblResult
    
    Exit Function
End Function

Public Function MMC_PZero(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer)

'Purpose:   Returns the probability of empty system in M/M/c system.
'           Gross & Harris, p86

'Arguments: dblArrivalRate     Mean of the Poisson arrival process
'           dblAvgServiceTime  Mean of exponential service time distribution
'           intNumServers      Number of servers (c)


'Returns:   Double             MMC empty system probability

'Author:    Mark Isken
'Created:   06/20/1997
'Modified:


Dim dblR As Double, dblFirstSum As Double, dblLastProduct As Double
Dim intCount As Integer

    dblR = dblArrivalRate * dblAvgServiceTime
    
    dblFirstSum = 0#
    For intCount = 0 To intNumServers - 1
        dblFirstSum = dblFirstSum + (dblR ^ intCount) / Application.Fact(intCount)
    Next intCount
    
    dblLastProduct = (intNumServers * (dblR ^ intNumServers)) / (Application.Fact(intNumServers) * (intNumServers - dblR))
    dblResult = 1 / (dblFirstSum + dblLastProduct)
    MMC_PZero = dblResult
    
    Exit Function
    
    
End Function

Public Function MMC_ProbWait_NormalApprox(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer)

'Purpose:   Returns an approximation to the probability of delay in M/M/c system.
'           Used normal approximation approach by Kolesar and Green, "Insights
'           on Service System Design from a Normal Approximation to Erlang's
'           Delay Formula", POM, V7, No3, Fall 1998, pp282-293

'Arguments: dblArrivalRate     Mean of the Poisson arrival process
'           dblAvgServiceTime  Mean of exponential service time distribution
'           intNumServers      Number of servers (c)


'Returns:   Double             MMC delay probability

'Author:    Mark Isken
'Created:   12/18/1998
'Modified:
   
Dim dblRho As Double, dblLoad As Double


    dblRho = dblArrivalRate * dblAvgServiceTime / intNumServers
    dblLoad = dblArrivalRate * dblAvgServiceTime
    
    If dblArrivalRate * dblAvgServiceTime > intNumServers Or dblArrivalRate * dblAvgServiceTime < 0 Then
        MMC_ProbWait_NormalApprox = CVErr(xlErrValue)
        Exit Function
    End If
    
    dblProbOfWait = 1 - Application.NormSDist((intNumServers - dblLoad - 0.5) / Sqr(dblLoad))
    MMC_ProbWait_NormalApprox = dblProbOfWait
    Exit Function
    
End Function

Public Function MDC_AvgQLength_CosmetatosApprox(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer) As Double

Dim dblRho As Double
Dim dblPiece1 As Double, dblPiece2 As Double, dblPiece3 As Double, dblPiece4 As Double, dblPiece5 As Double

    dblRho = dblArrivalRate * dblAvgServiceTime / intNumServers
    If dblArrivalRate * dblAvgServiceTime > intNumServers Or dblArrivalRate * dblAvgServiceTime < 0 Then
        MDC_AvgQLength_CosmetatosApprox = CVErr(xlErrValue)
        Exit Function
    End If
    
    dblPiece1 = 1 / 2
    dblPiece2 = 1
    dblPiece3 = (1 - dblRho) * (intNumServers - 1)
    dblPiece4 = (Sqr(4 + 5 * intNumServers) - 2) / (16 * intNumServers * dblRho)
    dblPiece5 = MMC_AvgWait(dblArrivalRate, dblAvgServiceTime, intNumServers) * dblArrivalRate
    
    MDC_AvgQLength_CosmetatosApprox = dblPiece1 * (dblPiece2 + dblPiece3 * dblPiece4) * dblPiece5
    Exit Function
    

End Function

Public Function MDC_AvgWait_CosmetatosApprox(dblArrivalRate As Double, dblAvgServiceTime As Double, intNumServers As Integer) As Double

Dim dblLq As Double

    If dblArrivalRate * dblAvgServiceTime > intNumServers Or dblArrivalRate * dblAvgServiceTime < 0 Then
        MDC_AvgWait_CosmetatosApprox = CVErr(xlErrValue)
        Exit Function
    End If
    
    dblLq = MDC_AvgQLength_CosmetatosApprox(dblArrivalRate, dblAvgServiceTime, intNumServers)
    MDC_AvgWait_CosmetatosApprox = dblLq / dblArrivalRate
    Exit Function
    
End Function

Public Function MGC_AvgWait_Approx(dblArrivalRate As Double, dblAvgServiceTime As Double, dblStdDevServiceTime As Double, intNumServers As Integer) As Double

Dim dblCVSquared As Double, dblWq As Double

    If dblArrivalRate * dblAvgServiceTime > intNumServers Or dblArrivalRate * dblAvgServiceTime < 0 Then
        MGC_AvgWait_Approx = CVErr(xlErrValue)
        Exit Function
    End If
    
    dblCVSquared = (dblStdDevServiceTime ^ 2) / (dblAvgServiceTime ^ 2)
    
    dblWq = (1 - dblCVSquared) * MDC_AvgWait_CosmetatosApprox(dblArrivalRate, dblAvgServiceTime, intNumServers)
    dblWq = dblWq + dblCVSquared * MMC_AvgWait(dblArrivalRate, dblAvgServiceTime, intNumServers)
    
    MGC_AvgWait_Approx = dblWq
    
    Exit Function
    
End Function

Public Function MGC_ConditionalWaitPercentile1(dblArrivalRate As Double, dblAvgServiceTime As Double, dblStdDevServiceTime As Double, intNumServers As Integer, dblProb As Double)

'Purpose:   Returns an approximation for the conditional p'th percentile of the M/G/c delay distribution.
'           The approximation is based on a first order approximation using the M/M/c delay percentile.
'           See Tijms, H.C. (1994), "Stochastic Models: An Algorithmic Approach", John Wiley and Sons, Chichester
'           Chapter 4, p299-300

'           The percentile is conditional on Wq>0 (i.e. on event customer waits)

'           This 1st order approximation is OK for 0<=CVSquared<=2 and dblProb>1-Prob(Delay)
'           Note that for Prob(Delay) we use MMC as approximation for same quantity in MGC.
'           Justification in Tijms (p296)

'Arguments: dblArrivalRate
'           dblAvgServiceTime
'           dblStdDevServiceTime
'           intNumServers
'           dblProb     Percentile desired

'Returns:   Double

'Author:    Mark Isken
'Created:   03/29/99
'Modified:

Dim dblMean As Double, dblCVSquared As Double, dblMMC_ProbWait As Double


    dblCVSquared = (dblStdDevServiceTime ^ 2) / (dblAvgServiceTime ^ 2)
    
    dblMean = dblArrivalRate * dblAvgServiceTime
    If dblMean <= 0# Then
        MGC_ConditionalWaitPercentile1 = CVErr(xlErrValue)
        Exit Function
    End If
    
    If dblProb <= 0 Or dblProb > 1# Then
        MGC_ConditionalWaitPercentile1 = CVErr(xlErrValue)
        Exit Function
    End If
    
    dblMMC_ProbWait = MMC_ProbWait(dblArrivalRate, dblAvgServiceTime, intNumServers)
    
End Function

Public Function BigPoisson(dblMean As Double, intAtom As Integer)

End Function

