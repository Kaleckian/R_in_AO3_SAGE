Attribute VB_Name = "Anomysation_Chart"
Option Explicit
Option Base 1
Sub Anon_ChartOfAccounts()

Dim rng As Range
Dim thing As Range
Dim ws As Worksheet
Dim StartRow As Long
Dim StrClose As String
Dim AccLevelColumn As Integer
Dim MaxAccLevel As Integer
Dim Str_AggLevel As String

'It is expected that the only active sheet will contain the chart of accounts.
Set ws = ThisWorkbook.ActiveSheet

'Manually, the name of the company is anonymised
ws.[A1] = "NEAT COMPANY"

' The eighth row is the very one after the name of the fields.
StartRow = 8
' The level of the account in the chart is at the forth column.
AccLevelColumn = 4

Set rng = Range(ws.Cells(StartRow, AccLevelColumn), ws.Cells(Rows.Count, AccLevelColumn).End(xlUp))

' The idea here is to use the useful field "Grau" (Brazilian version: level of the ledger account in the hierarchical chart)
' to hide the description of the analytical level. It is in the fourth column and it is supposed to be in numerical format.
' The assumption is that, obviously, all the "sensible" information is on the analytical level and changing the account's description
' at this level would suffice.

MaxAccLevel = WorksheetFunction.Max(rng)

For Each thing In rng
    If thing = MaxAccLevel - 1 Then
        Str_AggLevel = thing.Offset(, -1).Value

    ElseIf thing = MaxAccLevel Then
        thing.Offset(, -1).Value = Str_AggLevel
    Else

    End If

Next thing

End Sub

