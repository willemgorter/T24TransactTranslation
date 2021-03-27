*-----------------------------------------------------------------------------
* <Rating>320</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE E.NOFILE.BLM.TRANSLATION.EXTRACT(TDY.OUT)

*------------------------------------------------------------------------------*
*                                                                              *
* Subroutine to extract fields for Hebrew translation                          *
*                                                                              *
* and extracts are written away to :                                           *
*                                                                              *
*     ../bnk.interface/TRANSLATION/OUT                                         *
*                                                                              *
*------------------------------------------------------------------------------*

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.DATES
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.DICTIONARY
    $INSERT I_F.EB.ERROR
    $INSERT I_F.OVERRIDE
    $INSERT I_F.VERSION
    $INSERT I_F.ENQUIRY
    $INSERT I_F.HELPTEXT.MAINMENU
    $INSERT I_F.HELPTEXT.MENU
    $INSERT I_F.EB.LOOKUP
    $INSERT I_F.COUNTRY
    $INSERT I_CREATE.DICT.COMMON

    ! process control section
*
    GOSUB INITIALISE
*
    GOSUB GET.SELECTION
*
    GOSUB DO.EXTRACT.EB.DICTIONARY

    TDY.OUT = R.REC.TO.WRITE
    RETURN
*
*    GOSUB DO.EXTRACT.EB.ERROR
**
*    GOSUB DO.EXTRACT.OVERRIDE
**
*    GOSUB DO.EXTRACT.HELPTEXT.MAINMENU
**
*    GOSUB DO.EXTRACT.HELPTEXT.MENU
**
*    GOSUB DO.EXTRACT.EB.LOOKUP
**
*    GOSUB DO.EXTRACT.COUNTRIES

*using EB.DICTIONARY NO NEED TO EXTRACT VERSION AND ENQUIRY
*    GOSUB DO.EXTRACT.VERSION
*
*    GOSUB DO.EXTRACT.ENQUIRY
*
*    GOSUB DO.WRITE

*----------
INITIALISE:
*----------
*    CRT "Start extraction process"

    W.SEP = " # "
*    ! open files
*
*    ID.PAR = "BLM.TRANSLATION"
*
*    PAR.NAME = "PATH.OUT"
*    PAR.VALUE = ""
*    CALL ILMB.GET.GENERIC.PARAMETER(ID.PAR, PAR.NAME, PAR.VALUE, PAR.ATTRIB, S.ERR)
**
*    FN.TRANSLATION.OUT = PAR.VALUE
*    F.TRANSLATION.OUT  = ''
*    CALL OPF ( FN.TRANSLATION.OUT, F.TRANSLATION.OUT )
*
*    R.REC.TO.WRITE = ''
**build the file name
*    GOSUB FIND.NEXT.ID
**build HEADER
**    R.REC.TO.WRITE = "T24 APPLICATION NAME" : W.SEP : "T24 APPLICATION ID" : W.SEP : "T24 FIELD NAME.POS" : W.SEP : "VALUE"

    RETURN
*-------------
GET.SELECTION:
*-------------
    v.TDY.SelectionFields = ENQ.SELECTION<2>
    v.TDY.SelectionOperators = ENQ.SELECTION<3>
    v.TDY.SelectionValues = ENQ.SELECTION<4>
    CONVERT @VM TO @FM IN v.TDY.SelectionFields
    CONVERT @VM TO @FM IN v.TDY.SelectionOperators
    CONVERT @VM TO @FM IN v.TDY.SelectionValues

    LOCATE '@ID' IN v.TDY.SelectionFields SETTING POSN THEN
    ValueList = v.TDY.SelectionValues<POSN>
    Operand = v.TDY.SelectionOperators<POSN>
    END
    RETURN

********************************************************************************
DO.EXTRACT.EB.DICTIONARY:
********************************************************************************
*    CRT "Extracting EB.DICTIONARY..."
*    FN.EB.DICTIONARY = 'F.EB.DICTIONARY'
*    F.EB.DICTIONARY  = ''
*    CALL OPF (FN.EB.DICTIONARY, F.EB.DICTIONARY)
*
*    SEL.CMD = "SELECT " : FN.EB.DICTIONARY
*
*    IF Value THEN
*		SEL.CMD = SEL.CMD : " WITH DICT.KEY LIKE ":SQUOTE("...":Value:"...")
*	END
*
*    CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

*    CRT "Number records selected from EB.DICTIONARY:" : NB.SEL
    CONVERT @SM TO @FM IN ValueList
    ValueListCnt = DCOUNT(ValueList,@FM)
    FOR valueNum=1 TO ValueListCnt
        RecordId = ValueList<valueNum>
        CALL BLM.EXTRACT.EB.DICT(RecordId,EbDictRecordList)


        NB.SEL = DCOUNT(EbDictRecordList,@FM)

        FOR I = 1 TO NB.SEL
            ID.EB.DICTIONARY = EbDictRecordList<I,1>
            R.EB.DICTIONARY  = EbDictRecordList<I,2>
            R.EB.DICTIONARY = CHANGE(R.EB.DICTIONARY,"<fieldMarker>",@FM)
            R.EB.DICTIONARY = CHANGE(R.EB.DICTIONARY,"<valueMarker>",@VM)
            R.EB.DICTIONARY = CHANGE(R.EB.DICTIONARY,"<subValueMarker>",@SM)

            *
            IF R.EB.DICTIONARY<EB.DIC.DESCRIPTION> THEN
                R.REC.TO.WRITE<-1> = "EB.DICTIONARY" : W.SEP : ID.EB.DICTIONARY : W.SEP : "DESCRIPTION" : W.SEP : R.EB.DICTIONARY<EB.DIC.DESCRIPTION>
            END
            *
            IF R.EB.DICTIONARY<EB.DIC.SHORT.DESC> THEN
                R.REC.TO.WRITE<-1> = "EB.DICTIONARY" : W.SEP : ID.EB.DICTIONARY : W.SEP : "SHORT.DESC" : W.SEP : R.EB.DICTIONARY<EB.DIC.SHORT.DESC>
            END
            *
            IF R.EB.DICTIONARY<EB.DIC.TEXT> THEN
                R.REC.TO.WRITE<-1> = "EB.DICTIONARY" : W.SEP : ID.EB.DICTIONARY : W.SEP : "TEXT" : W.SEP : R.EB.DICTIONARY<EB.DIC.TEXT> : R.EB.DICTIONARY<EB.DIC.TXT.040..078> : R.EB.DICTIONARY<EB.DIC.TXT.079..117> : R.EB.DICTIONARY<EB.DIC.TXT.118..132>
            END
            *
            IF R.EB.DICTIONARY<EB.DIC.TOOL.TIP> THEN
                R.REC.TO.WRITE<-1> = "EB.DICTIONARY" : W.SEP : ID.EB.DICTIONARY : W.SEP : "TOOL.TIP" : W.SEP : R.EB.DICTIONARY<EB.DIC.TOOL.TIP>
            END
            *
            IF R.EB.DICTIONARY<EB.DIC.TXT.OPERATION> THEN
                R.REC.TO.WRITE<-1> = "EB.DICTIONARY" : W.SEP : ID.EB.DICTIONARY : W.SEP : "TXT.OPERATION" : W.SEP : R.EB.DICTIONARY<EB.DIC.TXT.OPERATION>
            END
            *
            IF R.EB.DICTIONARY<EB.DIC.PROMPT.TEXT> THEN
                R.REC.TO.WRITE<-1> = "EB.DICTIONARY" : W.SEP : ID.EB.DICTIONARY : W.SEP : "PROMPT.TEXT" : W.SEP : R.EB.DICTIONARY<EB.DIC.PROMPT.TEXT>
            END
            *
            IF R.EB.DICTIONARY<EB.DIC.ENQ.SEL.TEXT> THEN
                R.REC.TO.WRITE<-1> = "EB.DICTIONARY" : W.SEP : ID.EB.DICTIONARY : W.SEP : "ENQ.SEL.TEXT" : W.SEP : R.EB.DICTIONARY<EB.DIC.ENQ.SEL.TEXT>
            END
            *
        NEXT I
    NEXT valueNum

    RETURN

********************************************************************************
DO.EXTRACT.EB.ERROR:
********************************************************************************
*    CRT "Extracting EB.ERROR..."
    FN.EB.ERROR = 'F.EB.ERROR'
    F.EB.ERROR  = ''
    CALL OPF (FN.EB.ERROR, F.EB.ERROR)

    SEL.CMD = "SELECT " : FN.EB.ERROR

    CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

*    CRT "Number records selected from EB.ERROR:" : NB.SEL

    FOR I = 1 TO NB.SEL
        ID.EB.ERROR = SEL.RESULT<I>
        ERR.READ = ""
        CALL F.READ(FN.EB.ERROR, ID.EB.ERROR, R.EB.ERROR, F.EB.ERROR, ERR.READ)
        IF ERR.READ NE '' THEN
            CRT "READ ERROR EB.ERROR :" : ERR.READ : " ;FOR ID=" : ID.DICTIONARY
            GOSUB EXIT.POINT
        END ELSE
            IF R.EB.ERROR<EB.ERR.ERROR.MSG> THEN
                R.REC.TO.WRITE<-1> = "EB.ERROR" : W.SEP : ID.EB.ERROR : W.SEP : "ERROR.MSG.1.1" : W.SEP : R.EB.ERROR<EB.ERR.ERROR.MSG,1,1>
            END

        END
    NEXT I

    RETURN

********************************************************************************
DO.EXTRACT.OVERRIDE:
********************************************************************************
*    CRT "Extracting OVERRIDE..."
    FN.OVERRIDE = 'F.OVERRIDE'
    F.OVERRIDE  = ''
    CALL OPF (FN.OVERRIDE, F.OVERRIDE)

    SEL.CMD = "SELECT " : FN.OVERRIDE

    CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

*    CRT "Number records selected from OVERRIDE:" : NB.SEL

    FOR I = 1 TO NB.SEL
        ID.OVERRIDE = SEL.RESULT<I>
        ERR.READ = ""
        CALL F.READ(FN.OVERRIDE, ID.OVERRIDE, R.OVERRIDE, F.OVERRIDE, ERR.READ)
        IF ERR.READ NE '' THEN
            CRT "READ ERROR OVERRIDE :" : ERR.READ : " ;FOR ID=" : ID.DICTIONARY
            GOSUB EXIT.POINT
        END ELSE
            IF R.OVERRIDE<EB.OR.MESSAGE> THEN
                R.REC.TO.WRITE<-1> = "OVERRIDE" : W.SEP : ID.OVERRIDE : W.SEP : "MESSAGE.1.1" : W.SEP : R.OVERRIDE<EB.OR.MESSAGE>
            END

        END
    NEXT I

    RETURN

********************************************************************************
DO.EXTRACT.HELPTEXT.MAINMENU:
********************************************************************************
*    CRT "Extracting HELPTEXT.MAINMENU..."
    FN.HELPTEXT.MAINMENU = 'F.HELPTEXT.MAINMENU'
    F.HELPTEXT.MAINMENU  = ''
    CALL OPF (FN.HELPTEXT.MAINMENU, F.HELPTEXT.MAINMENU)

    SEL.CMD = "SELECT " : FN.HELPTEXT.MAINMENU

    CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

*    CRT "Number records selected from HELPTEXT.MAINMENU:" : NB.SEL

    FOR I = 1 TO NB.SEL
        ID.HELPTEXT.MAINMENU = SEL.RESULT<I>
        ERR.READ = ""
        CALL F.READ(FN.HELPTEXT.MAINMENU, ID.HELPTEXT.MAINMENU, R.HELPTEXT.MAINMENU, F.HELPTEXT.MAINMENU, ERR.READ)
        IF ERR.READ NE '' THEN
            CRT "READ ERROR HELPTEXT.MAINMENU :" : ERR.READ : " ;FOR ID=" : ID.DICTIONARY
            GOSUB EXIT.POINT
        END ELSE
            IF R.HELPTEXT.MAINMENU<EB.MME.TITLE> THEN
                R.REC.TO.WRITE<-1> = "HELPTEXT.MAINMENU" : W.SEP : ID.HELPTEXT.MAINMENU : W.SEP : "EB.MME.TITLE.1.1" : W.SEP : R.HELPTEXT.MAINMENU<EB.MME.TITLE>
            END
            NB.MENU = DCOUNT(R.HELPTEXT.MAINMENU<EB.MME.DESCRIPT>,VM)
            FOR II = 1 TO NB.MENU
                IF R.HELPTEXT.MAINMENU<EB.MME.DESCRIPT,II,1> THEN
                    R.REC.TO.WRITE<-1> = "HELPTEXT.MAINMENU" : W.SEP : ID.HELPTEXT.MAINMENU : W.SEP : "EB.MME.DESCRIPT." : II : ".1" : W.SEP : R.HELPTEXT.MAINMENU<EB.MME.DESCRIPT,II,1>
                END
            NEXT II

        END
    NEXT I

    RETURN

********************************************************************************
DO.EXTRACT.HELPTEXT.MENU:
********************************************************************************
*    CRT "Extracting HELPTEXT.MENU..."
    FN.HELPTEXT.MENU = 'F.HELPTEXT.MENU'
    F.HELPTEXT.MENU  = ''
    CALL OPF (FN.HELPTEXT.MENU, F.HELPTEXT.MENU)

    SEL.CMD = "SELECT " : FN.HELPTEXT.MENU

    CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

*    CRT "Number records selected from HELPTEXT.MENU:" : NB.SEL

    FOR I = 1 TO NB.SEL
        ID.HELPTEXT.MENU = SEL.RESULT<I>
        ERR.READ = ""
        CALL F.READ(FN.HELPTEXT.MENU, ID.HELPTEXT.MENU, R.HELPTEXT.MENU, F.HELPTEXT.MENU, ERR.READ)
        IF ERR.READ NE '' THEN
            CRT "READ ERROR HELPTEXT.MENU :" : ERR.READ : " ;FOR ID=" : ID.DICTIONARY
            GOSUB EXIT.POINT
        END ELSE
            NB.MENU = DCOUNT(R.HELPTEXT.MENU<EB.MEN.DESCRIPT>,SM)
            FOR II = 1 TO NB.MENU
                IF R.HELPTEXT.MENU<EB.MEN.DESCRIPT,II,1> THEN
                    R.REC.TO.WRITE<-1> = "HELPTEXT.MENU" : W.SEP : ID.HELPTEXT.MENU : W.SEP : "EB.MEN.DESCRIPT." : II : ".1" : W.SEP : R.HELPTEXT.MENU<EB.MEN.DESCRIPT,II,1>
                END
            NEXT II
        END
    NEXT I

    RETURN

********************************************************************************
DO.EXTRACT.EB.LOOKUP:
********************************************************************************
*    CRT "Extracting EB.LOOKUP..."
    FN.EB.LOOKUP = 'F.EB.LOOKUP'
    F.EB.LOOKUP  = ''
    CALL OPF (FN.EB.LOOKUP, F.EB.LOOKUP)

    SEL.CMD = "SELECT " : FN.EB.LOOKUP

    CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

*    CRT "Number records selected from EB.LOOKUP:" : NB.SEL

    FOR I = 1 TO NB.SEL
        ID.EB.LOOKUP = SEL.RESULT<I>
        ERR.READ = ""
        CALL F.READ(FN.EB.LOOKUP, ID.EB.LOOKUP, R.EB.LOOKUP, F.EB.LOOKUP, ERR.READ)
        IF ERR.READ NE '' THEN
            CRT "READ ERROR EB.LOOKUP :" : ERR.READ : " ;FOR ID=" : ID.DICTIONARY
            GOSUB EXIT.POINT
        END ELSE
            NB.EB.LOOKUP = DCOUNT(R.EB.LOOKUP<EB.LU.DESCRIPTION>,SM)
            FOR II = 1 TO NB.EB.LOOKUP
                IF R.EB.LOOKUP<EB.LU.DESCRIPTION,II,1> THEN
                    R.REC.TO.WRITE<-1> = "EB.LOOKUP" : W.SEP : ID.EB.LOOKUP : W.SEP : "EB.LU.DESCRIPTION." : II : ".1" : W.SEP : R.EB.LOOKUP<EB.LU.DESCRIPTION,II,1>
                END
            NEXT II
        END
    NEXT I

    RETURN

********************************************************************************
DO.EXTRACT.COUNTRIES:
********************************************************************************
*    CRT "Extracting COUNTRY..."
    FN.COUNTRY = 'F.COUNTRY'
    F.COUNTRY  = ''
    CALL OPF (FN.COUNTRY, F.COUNTRY)

    SEL.CMD = "SELECT " : FN.COUNTRY

    CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

*    CRT "Number records selected from COUNTRY:" : NB.SEL

    FOR I = 1 TO NB.SEL
        ID.COUNTRY = SEL.RESULT<I>
        ERR.READ = ""
        CALL F.READ(FN.COUNTRY, ID.COUNTRY, R.COUNTRY, F.COUNTRY, ERR.READ)
        IF ERR.READ NE '' THEN
            CRT "READ ERROR COUNTRY :" : ERR.READ : " ;FOR ID=" : ID.DICTIONARY
            GOSUB EXIT.POINT
        END ELSE
            NB.COUNTRY.NAME = DCOUNT(R.COUNTRY<EB.COU.COUNTRY.NAME>,SM)
            FOR II = 1 TO NB.COUNTRY.NAME
                IF R.COUNTRY<EB.COU.COUNTRY.NAME,II,1> THEN
                    R.REC.TO.WRITE<-1> = "COUNTRY" : W.SEP : ID.COUNTRY : W.SEP : "EB.COU.COUNTRY.NAME." : II : ".1" : W.SEP : R.COUNTRY<EB.COU.COUNTRY.NAME,II,1>
                END
            NEXT III
            NB.COUNTRY.SNAME = DCOUNT(R.COUNTRY<EB.COU.SHORT.NAME>,SM)
            FOR III = 1 TO NB.COUNTRY.SNAME
                IF R.COUNTRY<EB.COU.SHORT.NAME,III,1> THEN
                    R.REC.TO.WRITE<-1> = "COUNTRY" : W.SEP : ID.COUNTRY : W.SEP : "EB.COU.SHORT.NAME." : III : ".1" : W.SEP : R.COUNTRY<EB.COU.SHORT.NAME,III,1>
                END
            NEXT III
        END
    NEXT I

    RETURN

********************************************************************************
DO.EXTRACT.VERSION:
********************************************************************************
*    CRT "Extracting VERSION..."
    FN.VERSION = 'F.VERSION'
    F.VERSION  = ''
    CALL OPF (FN.VERSION, F.VERSION)

    SEL.CMD = "SELECT " : FN.VERSION

    CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

*    CRT "Number records selected from VERSION:" : NB.SEL

    FOR I = 1 TO NB.SEL
        ID.VERSION = SEL.RESULT<I>
        ERR.READ = ""
        CALL F.READ(FN.VERSION, ID.VERSION, R.VERSION, F.VERSION, ERR.READ)
        IF ERR.READ NE '' THEN
            CRT "READ ERROR VERSION :" : ERR.READ : " ;FOR ID=" : ID.DICTIONARY
            GOSUB EXIT.POINT
        END ELSE
            IF R.VERSION<EB.VER.HDR.1.001..039> THEN
                R.REC.TO.WRITE<-1> = "VERSION" : W.SEP : ID.VERSION : W.SEP : "EB.VER.HDR.1" : W.SEP : R.VERSION<EB.VER.HDR.1.001..039> : R.VERSION<EB.VER.HDR.1.040..078> : R.VERSION<EB.VER.HDR.1.079..117> : R.VERSION<EB.VER.HDR.1.118..132>
            END
            *
            NB.SUBVALUE = DCOUNT(R.VERSION<EB.VER.TOOL.TIP>,VM)
            IF R.VERSION<EB.VER.TOOL.TIP> THEN
                IF R.VERSION<EB.VER.TOOL.TIP,II,1> THEN
                    R.REC.TO.WRITE<-1> = "VERSION" : W.SEP : ID.VERSION : W.SEP : "EB.VER.TOOL.TIP." : II : ".1" : W.SEP : R.VERSION<EB.VER.TOOL.TIP,II,1>
                END
            END
            *
            NB.SUBVALUE = DCOUNT(R.VERSION<EB.VER.TEXT>,VM)
            FOR II = 1 TO NB.SUBVALUE
                IF R.VERSION<EB.VER.TEXT,II,1> THEN
                    R.REC.TO.WRITE<-1> = "VERSION" : W.SEP : ID.VERSION : W.SEP : "EB.VER.TEXT." : II : ".1" : W.SEP : R.VERSION<EB.VER.TEXT,II,1>
                END
            NEXT II
        END
    NEXT I

    RETURN

********************************************************************************
DO.EXTRACT.ENQUIRY:
********************************************************************************
*    CRT "Extracting ENQUIRY..."
    FN.ENQUIRY = 'F.ENQUIRY'
    F.ENQUIRY  = ''
    CALL OPF (FN.ENQUIRY, F.ENQUIRY)

    SEL.CMD = "SELECT " : FN.ENQUIRY

    CALL EB.READLIST(SEL.CMD,SEL.RESULT,'',NB.SEL,SEL.ERR)

*    CRT "Number records selected from ENQUIRY:" : NB.SEL

    FOR I = 1 TO NB.SEL
        ID.ENQUIRY = SEL.RESULT<I>
        ERR.READ = ""
        CALL F.READ(FN.ENQUIRY, ID.ENQUIRY, R.ENQUIRY, F.ENQUIRY, ERR.READ)
        IF ERR.READ NE '' THEN
            CRT "READ ERROR ENQUIRY :" : ERR.READ : " ;FOR ID=" : ID.DICTIONARY
            GOSUB EXIT.POINT
        END ELSE
            NB.SUBVALUE = DCOUNT(R.ENQUIRY<ENQ.SEL.LABEL>,VM)
            FOR II = 1 TO NB.SUBVALUE
                IF R.ENQUIRY<ENQ.SEL.LABEL,II,1> THEN
                    R.REC.TO.WRITE<-1> = "ENQUIRY" : W.SEP : ID.ENQUIRY : W.SEP : "ENQ.SEL.LABEL." : II : ".1" : W.SEP : R.ENQUIRY<ENQ.SEL.LABEL,II,1>
                END
            NEXT II
            *
            NB.SUBVALUE = DCOUNT(R.ENQUIRY<ENQ.FIELD.LBL>,VM)
            FOR II = 1 TO NB.SUBVALUE
                IF R.ENQUIRY<ENQ.FIELD.LBL,II,1> THEN
                    R.REC.TO.WRITE<-1> = "ENQUIRY" : W.SEP : ID.ENQUIRY : W.SEP : "ENQ.FIELD.LBL." : II : ".1" : W.SEP : R.ENQUIRY<ENQ.FIELD.LBL,II,1>
                END
            NEXT II
            *
            NB.SUBVALUE = DCOUNT(R.ENQUIRY<ENQ.OPERATION>,VM)
            FOR II = 1 TO NB.SUBVALUE
                IF R.ENQUIRY<ENQ.OPERATION,II,1> THEN
                    R.REC.TO.WRITE<-1> = "ENQUIRY" : W.SEP : ID.ENQUIRY : W.SEP : "ENQ.OPERATION." : II : ".1" : W.SEP : R.ENQUIRY<ENQ.OPERATION,II>
                END
            NEXT II
            *
            IF R.ENQUIRY<ENQ.LABEL.FIELD> THEN
                R.REC.TO.WRITE<-1> = "ENQUIRY" : W.SEP : ID.ENQUIRY : W.SEP : "ENQ.LABEL.FIELD" : W.SEP : R.ENQUIRY<ENQ.LABEL.FIELD>
            END
            *
            IF R.ENQUIRY<ENQ.NXT.DESC> THEN
                R.REC.TO.WRITE<-1> = "ENQUIRY" : W.SEP : ID.ENQUIRY : W.SEP : "ENQ.NXT.DESC" : W.SEP : R.ENQUIRY<ENQ.NXT.DESC>
            END
            *
            IF R.ENQUIRY<ENQ.DESCRIPT> THEN
                R.REC.TO.WRITE<-1> = "ENQUIRY" : W.SEP : ID.ENQUIRY : W.SEP : "ENQ.DESCRIPT.1.1" : W.SEP : R.ENQUIRY<ENQ.DESCRIPT,1,1>
            END
            *
            IF R.ENQUIRY<ENQ.SHORT.DESC> THEN
                R.REC.TO.WRITE<-1> = "ENQUIRY" : W.SEP : ID.ENQUIRY : W.SEP : "ENQ.SHORT.DESC.1" : W.SEP : R.ENQUIRY<ENQ.SHORT.DESC,1>
            END
        END
    NEXT I

    RETURN


********************************************************************************
DO.WRITE:
********************************************************************************
*write export file
    CALL F.WRITE(FN.TRANSLATION.OUT, Y.FILE.NAME, R.REC.TO.WRITE)

    CALL JOURNAL.UPDATE("")

    RETURN


********************************************************************************
FIND.NEXT.ID:
********************************************************************************
*File name: T24.EXTRACT.TO.TRANSLATEDDMMYY
    SEQ.NR = 0
    Y.FILE.NAME = "T24.EXTRACT.TO.TRANSLATE_" : TODAY[7,2] : TODAY[5,2] : TODAY[1,4]
    LOOP
        CALL F.READ(FN.TRANSLATION.OUT, Y.FILE.NAME, R.FILE, F.TRANSLATION.OUT, ERR.READ)
        IF ERR.READ EQ '' THEN
            SEQ.NR++
            Y.FILE.NAME = "T24.EXTRACT.TO.TRANSLATE_" : TODAY[7,2] : TODAY[5,2] : TODAY[1,4] : "-" : FMT(SEQ.NR,'3"0"R')
        END
    WHILE ERR.READ EQ ''
    REPEAT

    RETURN


********************************************************************************
EXIT.POINT:
********************************************************************************

    STOP

    END
