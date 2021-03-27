*-----------------------------------------------------------------------------
* <Rating>480</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE BLM.TRANSLATION.RETRIVE(RETRIVE.PARAMS,LINES.TO.TRANSLATE)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STANDARD.SELECTION
*-----------------------------------------------------------------------------
    LINES.TO.TRANSLATE = ""
    W.SEP = @VM
    APPL = RETRIVE.PARAMS<1>
    ID.OPERA = RETRIVE.PARAMS<2,1>
    ID.PARAM = RETRIVE.PARAMS<2,2>
    IF ID.OPERA EQ 'LK' THEN ID.OPERA = 'LIKE'
    CALL GET.STANDARD.SELECTION.DETS(APPL,Y.SS.REC)
    NO.OF.FIELDS = DCOUNT(Y.SS.REC<SSL.SYS.FIELD.NAME>,@VM)
    FIELDS.TO.TRANSLATE = ''
    FOR YJI=1 TO NO.OF.FIELDS
        IF Y.SS.REC<SSL.SYS.LANG.FIELD,YJI> NE 'N' AND Y.SS.REC<SSL.SYS.LANG.FIELD,YJI> NE '' THEN
            FIELDS.TO.TRANSLATE<-1> = Y.SS.REC<SSL.SYS.FIELD.NO,YJI,1> : @VM : Y.SS.REC<SSL.SYS.LANG.FIELD,YJI> : @VM : Y.SS.REC<SSL.SYS.FIELD.NAME,YJI>
        END
    NEXT YJI

    IF FIELDS.TO.TRANSLATE NE '' THEN
        CRT "Extracting " : APPL : "..."
        FN.APPL = 'F.':APPL
        F.APPL  = ''
        CALL OPF (FN.APPL, F.APPL)
        IF ID.OPERA EQ '' AND ID.PARAM EQ '' THEN
            SEL.CMD = "SELECT " : FN.APPL
        END ELSE
			SEL.CMD = "SELECT " : FN.APPL :" WITH @ID ":ID.OPERA:" ":SQUOTE(ID.PARAM)
        END
        CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

        CRT "Number records selected from ":APPL:": " : NB.SEL

        FOR I = 1 TO NB.SEL
            ID.APPL = SEL.RESULT<I>
            ERR.READ = ""
            CALL F.READ(FN.APPL, ID.APPL, R.APPL, F.APPL, ERR.READ)
            IF ERR.READ NE '' THEN
                CRT "READ ERROR ":APPL:" :" : ERR.READ : " ;FOR ID=" : ID.APPL
                STOP
            END ELSE
                FTF.CNT = DCOUNT(FIELDS.TO.TRANSLATE,@FM)
                FOR YII=1 TO FTF.CNT
                    YFIELD.NO = FIELDS.TO.TRANSLATE<YII,1>
                    YFIELD.LANG.TYPE = FIELDS.TO.TRANSLATE<YII,2>
                    YFIELD.LANG.NAME = FIELDS.TO.TRANSLATE<YII,3>
                    IF YFIELD.LANG.TYPE EQ "Y" THEN
                        LINES.TO.TRANSLATE<-1> = APPL : W.SEP : ID.APPL : W.SEP :  YFIELD.LANG.NAME:".1" : W.SEP : R.APPL<YFIELD.NO,1> : W.SEP : R.APPL<YFIELD.NO,2>
                    END ELSE IF YFIELD.LANG.TYPE EQ "S" THEN
                        NB.CNT = DCOUNT(R.APPL<YFIELD.NO>,SM)
                        FOR II=1 TO NB.CNT
                            IF R.APPL<YFIELD.NO,II,1> THEN
                                LINES.TO.TRANSLATE<-1> = APPL : W.SEP : ID.APPL : W.SEP :  YFIELD.LANG.NAME:"." : II : ".1" : W.SEP : R.APPL<YFIELD.NO,II,1> : W.SEP :  R.APPL<YFIELD.NO,II,2>
                            END
                        NEXT II
                    END
                NEXT YII
            END
        NEXT I
    END

    RETURN
    
*-----------------------------------------------------------------------------
OPERAND:
*-----------------------------------------------------------------------------

   BEGIN CASE
       CASE SEL.OPR = 1
           SEL.OPR = 'EQ'
       CASE SEL.OPR = 2
           SEL.OPR = 'RG'
       CASE SEL.OPR = 3
           SEL.OPR = 'LT'
       CASE SEL.OPR = 4
           SEL.OPR = 'GT'
       CASE SEL.OPR = 5
           SEL.OPR = 'NE'
       CASE SEL.OPR = 6
           SEL.OPR = 'LIKE'
       CASE SEL.OPR = 7
           SEL.OPR = 'UNLIKE'
       CASE SEL.OPR = 8
           SEL.OPR = 'LE'
       CASE SEL.OPR = 9
           SEL.OPR = 'GE'
       CASE SEL.OPR = 10
           SEL.OPR = 'NR'
   END CASE

   RETURN
    
  
    END
