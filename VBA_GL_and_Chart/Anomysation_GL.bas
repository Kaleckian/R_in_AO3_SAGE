Attribute VB_Name = "Anomysation_GL"
Option Explicit
Option Base 1
Sub Anon_GeneralLedger()

Dim rng As Range
Dim thing As Range
Dim ws As Worksheet
Dim StartRow As Long
Dim ColumnEvents As Integer
Dim StrClose As String

'It is expected that the only active sheet will contain the general ledeger.
Set ws = ThisWorkbook.ActiveSheet

'Manually, the name of the company and its CPNJ (Corporate Register Number) are anonymised
ws.[A1] = "NEAT COMPANY"
ws.[A4] = "123456789"

' The eighth row is the very one after the name of the field.
StartRow = 8
' Accounting event/description column
ColumnEvents = 2

Set rng = Range(ws.Cells(StartRow, ColumnEvents), ws.Cells(Rows.Count, ColumnEvents).End(xlUp))

' Ledger entries with accounting events othen than "TRANSF RESULT" (string that defines the closure of the
' fiscal year in this particular Brazilian case) are anomymised.
' Relying on knowing that this particular string identifies such entry may be problematic.
' If one has the support of the accounting or internal IT teams, it is possible to unequivocally
' find them by automated lots and sub-lots entries (their codes).

StrClose = "TRANSF RESULT"

For Each thing In rng
    If InStr(1, UCase(thing), "TRANSF RESULT") <> 0 Then
        
    ElseIf thing = "" Then
    
    Else
        thing = "XPTO"
    End If
    
Next thing

End Sub

