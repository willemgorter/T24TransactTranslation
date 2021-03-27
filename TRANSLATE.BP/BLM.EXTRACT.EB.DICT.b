*-----------------------------------------------------------------------------
* <Rating>-311</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE BLM.EXTRACT.EB.DICT(RecordId,EbDictRecordList)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.VERSION
    $INSERT I_F.ENQUIRY
    $INSERT I_F.EB.DICTIONARY
    $INSERT I_BATCH.FILES
    $INSERT I_CREATE.DICT.COMMON
    $INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------

    GOSUB INIT      ;* initialise all variables required

    BEGIN CASE
        CASE CREATE.VERSION ;* Dictionary For Version
            VERSION.NAME = FIELD(CONTRACT.KEY,"F.VERSION>",2,1) ;*Get the Version Id
            GOSUB CREATE.DICT.FOR.VERSION   ;* Create Dictionary record
        CASE CREATE.ENQUIRY  ;* Dictionary for Enquiry
            ENQUIRY.NAME = FIELD(CONTRACT.KEY,"F.ENQUIRY>",2,1) ;*Get Enquiry key
            GOSUB CREATE.DICT.FOR.ENQUIRY   ;* Create Dictionary record
    END CASE

    RETURN
*-----------------------------------------------------------------------------

*** <region name= INIT>
INIT:
*** <desc>Initialise all variables required</desc>
		
    IF INDEX(RecordId,",",1) THEN
        CONTRACT.KEY = "F.VERSION>":RecordId
    END ELSE
        CONTRACT.KEY = "F.ENQUIRY>":RecordId
    END
	EbDictRecordList = ''
    CREATE.VERSION = INDEX(CONTRACT.KEY,"F.VERSION>",1) ;* Set flag when key contains Version filename
    CREATE.ENQUIRY = INDEX(CONTRACT.KEY,"F.ENQUIRY>",1) ;* Set Flag when key contain enquiry filename

    R.VERS= '' ;* Holds the Version Record
    R.ENQUIRY = '' ; * Holds the Enquiry record
    VERS.ERR = '' ;* Holds error information of Version
    ENQ.ERR = '' ;* Holds error information of Enquiry
    FN.VERSION = "F.VERSION" ; F.VERSION = "" ; CALL OPF(FN.VERSION,F.VERSION)
    FN.ENQUIRY = "F.ENQUIRY" ; F.ENQUIRY = "" ; CALL OPF(FN.ENQUIRY,F.ENQUIRY)

    RETURN
*** </region>
*-----------------------------------------------------------------------------
*** <region name= CREATE.DICT.FOR.VERSION>
CREATE.DICT.FOR.VERSION:
*** <desc>Create Dictionary for headers and Fields in Version</desc>

    CALL F.READ(FN.VERSION,VERSION.NAME,R.VERS,F.VERSION,VERS.ERR)    ; * Read Version record
	IF R.VERS<EB.VER.LANGUAGE.CODE> EQ '' THEN
		 R.VERS<EB.VER.LANGUAGE.CODE> = '1'
	END
    IF NOT(R.VERS) THEN     ;* Invalid version record specified
        RETURN     ;*dont create dictionary
    END
    IF R.VERS THEN    ;* when version record exists
        LANG.CODE = R.VERS<EB.VER.LANGUAGE.CODE>   ;* get the language code from Version
        NO.OF.LANG = DCOUNT(LANG.CODE,VM)        ;* Count of Language codes
        GOSUB CREATE.DICT.FOR.VERS.HEADERS        ;* Create Dictionary for Headers and Description of Version
        GOSUB CREATE.DICT.FOR.FIELDS ; *  Create Dictionary for Field nos in Version
    END

    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= CREATE.DICT.FOR.ENQUIRY>
CREATE.DICT.FOR.ENQUIRY:
*** <desc>Create Dictionary For Headers and fields in Enquiry</desc>

    CALL F.READ(FN.ENQUIRY,ENQUIRY.NAME,R.ENQUIRY,F.ENQUIRY,ENQ.ERR) ;* Read enquiry record
    IF NOT(R.ENQUIRY) THEN   ;* Invalid Version specified
        RETURN    ;* dont create Dictionary
    END

    GOSUB CREATE.DICT.FOR.ENQUIRY.FIELDS ; * Create Dictionary record for field labels in enquiry
    GOSUB CREATE.DICT.FOR.ENQ.SELECT.FLDS ; * Dictionary for Selection labels
    GOSUB CREATE.DICT.FOR.ENQ.TOOLS ; * Dictionary for Tool Text
    GOSUB CREATE.DICT.FOR.NXT.DESC ; * Dictionary for Next description in enquiry
    GOSUB CREATE.DICT.FOR.ENQ.HEADERS ; * Dictionary for Enquiry headers,Description and Short description

    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= CREATE.DICT.FOR.VERS.HEADERS>
CREATE.DICT.FOR.VERS.HEADERS:
*** <desc>Creation of Dictionary record For Header and Description of Version</desc>

    R.DICT = ''     ;* Variable to hold Dictionary record

    FOR SMC = 1 TO NO.OF.LANG    ;* For each language in Version
        LANG.SMC = LANG.CODE<1,SMC>    ;* Get the language code
        LANG.MNE = T.LANGUAGE<LANG.SMC>     ;* Form the language mnemonic for it
        DICT.KEY = VERSION.NAME      ;* Key should be version name
        R.DICT<EB.DIC.HDR.1.001..039> = R.VERS<EB.VER.HDR.1.001..039,SMC>  ;* Set the Header details from Version
        R.DICT<EB.DIC.HDR.1.040..078> = R.VERS<EB.VER.HDR.1.040..078,SMC>
        R.DICT<EB.DIC.HDR.1.079..117> = R.VERS<EB.VER.HDR.1.079..117,SMC>
        R.DICT<EB.DIC.HDR.1.118..132> = R.VERS<EB.VER.HDR.1.118..132,SMC>
        R.DICT<EB.DIC.HDR.2.001..039> = R.VERS<EB.VER.HDR.2.001..039,SMC>
        R.DICT<EB.DIC.HDR.2.040..078> = R.VERS<EB.VER.HDR.2.040..078,SMC>
        R.DICT<EB.DIC.HDR.2.079..117> = R.VERS<EB.VER.HDR.2.079..117,SMC>
        R.DICT<EB.DIC.HDR.2.118..132> = R.VERS<EB.VER.HDR.2.118..132,SMC>
        R.DICT<EB.DIC.DESCRIPTION> = R.VERS<EB.VER.DESCRIPTION,LANG.SMC>   ;*Set the Description of Version
        GOSUB WRITE.DICTIONARY    ;* Write information into Dictionary
    NEXT SMC

* Description is language based field,no of languages defined may not match with version language code
    NO.OF.DESCRIPTION = DCOUNT(R.VERS<EB.VER.DESCRIPTION>,VM)   ;* No of Description for Version
    FOR DESC.NO = 1 TO NO.OF.DESCRIPTION     ;* For each description
        IF NOT(DESC.NO MATCHES LANG.CODE) THEN    ;* When it is not previously created
            LANG.MNE = T.LANGUAGE<DESC.NO>    ;* Form the language mnemonic
            DICT.KEY = VERSION.NAME     ;* form dictionary key with Version name
            R.DICT<EB.DIC.DESCRIPTION> = R.VERS<EB.VER.DESCRIPTION,DESC.NO>  ;* Set the Description of Version
            GOSUB WRITE.DICTIONARY    ;* Write information in to Dictionary
        END
    NEXT DESC.NO

    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= WRITE.DICTIONARY>
WRITE.DICTIONARY:
*** <desc>Write information in to Dictionary</desc>

    IF LANG.MNE NE T.LANGUAGE<1> THEN   ;* Not an english language
        DICT.KEY = DICT.KEY:"-":LANG.MNE ;* Append the language mnemonic with Key
    END

*    SAVE.OFS.ID = OFS$SOURCE.ID    ;* save ofs source id
*    SAVE.OFS.REC= OFS$SOURCE.REC   ;* save ofs record
    R.DICT.SAVE  = R.DICT        ;* save Dictionary record created

    CONVERT FM TO '' IN R.DICT.SAVE   ;* Change FM/VM/SM markers to null
    CONVERT VM TO '' IN R.DICT.SAVE
    CONVERT SM TO '' IN R.DICT.SAVE

    IF NOT(R.DICT.SAVE) OR NOT(DICT.KEY) THEN     ;* Dont allow null records to be created
        RETURN      ;* Skip the field
    END

*    OFS$SOURCE.ID = OFS.SOURCE.ID   ;* Ofs source id
*    OFS$SOURCE.REC = R.OFS.SOURCE   ;* Ofs source record
*
*    NO.OF.AUTH = ''        ;* no of authorisers
*    CALL F.READ(FN.EB.DICTIONARY,DICT.KEY,R.DICT.LIVE,F.EB.DICTIONARY,DIC.ERR)   ;* Read Dictionary record
*    IF R.DICT.LIVE THEN      ;*When live file exists,write information in Unauth file
*        NO.OF.AUTH = 1     ;* Move it to live file
*    END ELSE
*        NO.OF.AUTH = 0    ;* Move it to Unauth file
*    END
*    CONVERT "," TO "?" IN DICT.KEY       ;* Change comma to "?"
    DICT.VERSION = "EB.DICTIONARY," ; * Version to upload Dictionary
    OFS.RECORD = '' ;* Store ofs message
    R.DICT = CHANGE(R.DICT,@FM,"<fieldMarker>")
    R.DICT = CHANGE(R.DICT,@VM,"<valueMarker>")
    R.DICT = CHANGE(R.DICT,@SM,"<subValueMarker>")
    EbDictRecordList<-1> = DICT.KEY : @VM : R.DICT
    
*    CALL OFS.BUILD.RECORD("EB.DICTIONARY","I","PROCESS",DICT.VERSION,'',NO.OF.AUTH,DICT.KEY,R.DICT,OFS.RECORD) ;*Build Ofs message
*
*
*    REQUEST = OFS.RECORD  ;* send it as a Request to OBM
*    RESPONSE = ''     ;* Response of request
*    REQUEST.COMMIT = '' ;*  Commit information of Request
*    CALL OFS.BULK.MANAGER(REQUEST, RESPONSE, REQUEST.COMMIT)      ;* Call OBM
*    OFS$SOURCE.ID = SAVE.OFS.ID    ;* restore ofs source id
*    OFS$SOURCE.REC = SAVE.OFS.REC  ;* Restore ofs record
*
*    IF NOT(REQUEST.COMMIT) THEN    ;* Error in ofs creation
*        GOTO PROGRAM.ABORT          ;* Get out of here immediately
*    END



    RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= CREATE.DICT.FOR.FIELDS>
CREATE.DICT.FOR.FIELDS:
*** <desc>Create Dictionary for field text defined in Version </desc>

    FIELD.NAMES.LIST = R.VERS<EB.VER.FIELD.NO>          ;* get the field names in version
    NO.OF.FIELDS = DCOUNT(R.VERS<EB.VER.FIELD.NO>,VM)   ;* no of fields in version
    INDEX.NO = 1       ;* used to store the index no of "*" fields in Version

    FOR VMC = 1 TO NO.OF.FIELDS    ;* Loop through all fields
        FULL.FLD.NAME = FIELD.NAMES.LIST<1,VMC>   ;* get the field name
        FLD.NAME = FIELD(FULL.FLD.NAME,"-",1) ;* Get the field name without multivalue and subvalue no
        FOR SMC = 1 TO NO.OF.LANG      ;* For all languages
            LANG.SMC = LANG.CODE<1,SMC>
            LANG.MNE = T.LANGUAGE<LANG.SMC>   ;* get the language mnemonic
            R.DICT = ''  ;*initialise dictionary record variable
            IF FLD.NAME = "*" THEN      ;* When field name as "*" in it
                FLD.NAME = "LABEL*":INDEX.NO  ;* Form the field name as LABEL*1 or LABEL*2 depending upon no of "*" fields
                INDEX.NO +=1   ;* increment Index no
            END
            IF FLD.NAME = "LOCAL.REF" THEN  ;* when field is a local reference field
                FLD.NAME = FULL.FLD.NAME     ;* Dont strip off the local reference no
            END
            R.DICT<EB.DIC.TEXT> = R.VERS<EB.VER.TEXT,VMC,SMC>  ;* Get the Text information From Dictionary
            R.DICT<EB.DIC.TOOL.TIP> = R.VERS<EB.VER.TOOL.TIP,VMC,SMC>  ;* Get the Tooltip From Dictionary
            R.DICT<EB.DIC.TXT.040..078> = R.VERS<EB.VER.TXT.040..078,VMC,SMC> ;* Get the Text information From Dictionary
            R.DICT<EB.DIC.TXT.079..117> = R.VERS<EB.VER.TXT.079..117,VMC,SMC> ;* Get the Text information From Dictionary
            R.DICT<EB.DIC.TXT.118..132> = R.VERS<EB.VER.TXT.118..132,VMC,SMC> ;* Get the Text information From Dictionary
            R.DICT<EB.DIC.PROMPT.TEXT> = R.VERS<EB.VER.PROMPT.TEXT,VMC,SMC> ;* Get the Prompt Text information From Dictionary
            DICT.KEY = VERSION.NAME:"-":FLD.NAME    ;*Dict key will be Version.name-field.name-Languagemnemonic
            GOSUB WRITE.DICTIONARY  ;*Write information in to dictionary
        NEXT SMC
    NEXT VMC
    RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= CREATE.DICT.FOR.ENQUIRY.FIELDS>
CREATE.DICT.FOR.ENQUIRY.FIELDS:
*** <desc>Create Dictionary record for Enquiry Field labels and Operation with Text information </desc>

    NO.OF.FIELDS = DCOUNT(R.ENQUIRY<ENQ.FIELD.NAME>,VM) ;* Count of Field names in Enquiry

    FOR FLD.NO = 1 TO NO.OF.FIELDS
        FIELD.NAME = R.ENQUIRY<ENQ.FIELD.NAME,FLD.NO>      ;*Get the Field name of Enquiry
        FIELD.LBL = R.ENQUIRY<ENQ.FIELD.LBL,FLD.NO>       ;*get the field label details from Enquiry
        FIELD.OPERATION = R.ENQUIRY<ENQ.OPERATION,FLD.NO>   ;*Get Field Operation details
        R.DICT = ''    ;* initialise Dictionary record
        R.DICT.OPERATION = '' ;* initialise Dictionary record which holds operation information
        IF (FIELD.OPERATION[1,1] EQ "'") OR (FIELD.OPERATION[1,1] EQ '"') THEN     ;*check if operation is header
            IF NOT(NUM(FIELD.OPERATION)) THEN  ;* Check if it is a numeric character
                NO.OF.FLD.OPER = DCOUNT(FIELD.OPERATION,SM) ;* get the no of operations
                FOR OPER = 1 TO NO.OF.FLD.OPER  ;* loop through each sub value
                    LEN.OPER = LEN(FIELD.OPERATION<1,1,OPER>) - 2 ;*Get the length to strip off Quotes
                    R.DICT<EB.DIC.TXT.OPERATION,OPER> = FIELD.OPERATION<1,1,OPER>[2,LEN.OPER] ;* get the operation
                NEXT OPER
                R.DICT.OPERATION = R.DICT     ;* Store it in Array
            END
        END

        NO.OF.LANG = DCOUNT(FIELD.LBL,SM) ;*get no of languages as per the field label
        FOR SMC = 1 TO NO.OF.LANG ;* loop through all languages
            LANG.MNE = T.LANGUAGE<SMC>   ;* get language mnemonic
            DICT.KEY = ENQUIRY.NAME :'-':FIELD.NAME ;* Form dictionary key as Enquiryname-fieldname-languagemnemonic
            R.DICT<EB.DIC.TEXT> = FIELD.LBL<1,1,SMC>  ;* Put it in Text
            GOSUB WRITE.DICTIONARY ;*Write information in to Dictionary
        NEXT SMC

        IF NOT(NO.OF.LANG) AND R.DICT.OPERATION THEN  ;* Operation without any field label
            LANG.MNE = T.LANGUAGE<1>     ; * Update operation in English
            R.DICT = R.DICT.OPERATION    ;* Set Dictionary record with operation
            DICT.KEY = ENQUIRY.NAME:'-':FIELD.NAME ;* Form dictionary key without language mnemonic
            GOSUB WRITE.DICTIONARY ;*Write Operation information in to Dictionary
        END

    NEXT FLD.NO
    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= CREATE.DICT.FOR.ENQ.SELECT.FLDS>
CREATE.DICT.FOR.ENQ.SELECT.FLDS:
*** <desc> Create Dictionary record for Selection labels </desc>

    NO.OF.SEL.FIELDS = DCOUNT(R.ENQUIRY<ENQ.SELECTION.FLDS>,VM) ; *no of selection fields in enquiry

    FOR SEL.FLD.NO = 1 TO NO.OF.SEL.FIELDS      ;* for each selection field
        SEL.FIELD.NAME = R.ENQUIRY<ENQ.SELECTION.FLDS,SEL.FLD.NO>    ;* get selection field name
        SEL.LBL = R.ENQUIRY<ENQ.SEL.LABEL,SEL.FLD.NO>     ;* Selection label for selection field
        R.DICT = ''    ;* Initialise Dictionary record
        NO.OF.LANG = DCOUNT(SEL.LBL,SM)    ;*no of languages based on selection label
        FOR SMC = 1 TO NO.OF.LANG   ;* For each language
            LANG.MNE = T.LANGUAGE<SMC>    ;* get the language mnemonic
            DICT.KEY = ENQUIRY.NAME:'-':SEL.FIELD.NAME  ;* Form Dictionary key as enquiry-selectionfield-language mnemonic
            R.DICT<EB.DIC.ENQ.SEL.TEXT> = SEL.LBL<1,1,SMC> ;* Set Selection text with selection label
            GOSUB WRITE.DICTIONARY  ;* Write information into dictionary record
        NEXT SMC
    NEXT SEL.FLD.NO

    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= CREATE.DICT.FOR.ENQ.TOOLS>
CREATE.DICT.FOR.ENQ.TOOLS:
*** <desc>Create Dictionary record for Tool Text in Enquiry</desc>
    NO.OF.TOOL.ID = DCOUNT(R.ENQUIRY<ENQ.TOOL.ID>,VM) ;* count of tool ids

    FOR TOOL.NO = 1 TO NO.OF.TOOL.ID  ;* for each tool id
        TOOL.ID = R.ENQUIRY<ENQ.TOOL.ID,TOOL.NO>     ;* get TOOL.ID
        TOOL.TEXT = R.ENQUIRY<ENQ.TOOL.TEXT,TOOL.NO> ;* get corresponding tool text
        R.DICT = '' ;* Initialise Dictionary record
        NO.OF.LANG = DCOUNT(TOOL.TEXT,SM)  ;* no of languages based on each tool text
        FOR SMC = 1 TO NO.OF.LANG ;* For all languages
            LANG.MNE = T.LANGUAGE<SMC> ;* language mnemonic for language
            DICT.KEY = ENQUIRY.NAME:'-':TOOL.ID   ;* Dictionary key
            R.DICT<EB.DIC.ENQ.TOOL.TEXT> = TOOL.TEXT<1,1,SMC> ;* Set enquiry tool text in dictionary
            GOSUB WRITE.DICTIONARY ;* Write information into dictionary record
        NEXT SMC
    NEXT TOOL.NO

    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= CREATE.DICT.FOR.NXT.DESC>
CREATE.DICT.FOR.NXT.DESC:
*** <desc>Create Dictionary record for Nxt Description in Enquiry</desc>

    NO.OF.NEXT.DESC = DCOUNT(R.ENQUIRY<ENQ.NXT.DESC>,VM) ;* get count of nxt description in Enquiry
    FOR NEXT.NO = 1 TO NO.OF.NEXT.DESC  ;* For each next description
        NO.OF.LANG = DCOUNT(R.ENQUIRY<ENQ.NXT.DESC,NEXT.NO>,SM) ;*Set language based on each nxt description
        FOR NXT.DESC = 1 TO NO.OF.LANG ;* Loop through languages
            R.DICT = ''      ;* Initialise Dictionary record
            LANG.MNE = T.LANGUAGE<NXT.DESC> ;* get language mnemonic
            NEXT.DESCRIPTION.TEXT = R.ENQUIRY<ENQ.NXT.DESC,NEXT.NO,NXT.DESC> ;* Get Next description
            NEXT.DESCRIPTION.TEXT = FIELD(NEXT.DESCRIPTION.TEXT,"[",1) ;* Next description without .gif information
            IF NXT.DESC = 1 THEN  ;* For english
                DESC.KEY = NEXT.DESCRIPTION.TEXT ;* get the text
                CONVERT " " TO "_" IN DESC.KEY ;*conver spaces to underscore
            END
            DICT.KEY = DESC.KEY  ;* key will be English translation with language mnemonic
            R.DICT<EB.DIC.TEXT> = NEXT.DESCRIPTION.TEXT  ;* Set Text of Dictionary
            GOSUB WRITE.DICTIONARY ;*write information into Dictionary
        NEXT NXT.DESC
    NEXT NEXT.NO
    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= CREATE.DICT.FOR.ENQ.HEADERS>
CREATE.DICT.FOR.ENQ.HEADERS:
*** <desc>Create Dictionary record For Header,Short.desc and Description of Version</desc>


    R.DICT.HEADER = '' ;* Initialise variable to hold the header information
    GOSUB FORM.HEADER ; * Form header details

    NO.OF.LANG.DESC = DCOUNT(R.ENQUIRY<ENQ.DESCRIPT,1>,SM) ;*no of languages based on Description
    FOR SMC = 1 TO NO.OF.LANG.DESC ;* for each language
        R.DICT = ''      ;* Intialise Dictionary record
        ENQ.LANG.CODE<1,-1> = SMC ;* Store corresponding language code
        LANG.MNE =  T.LANGUAGE<SMC> ;* set language mnemonic
        NO.OF.DESC = DCOUNT(R.ENQUIRY<ENQ.DESCRIPT>,VM) ;* no of Description in a language
        DICT.KEY = ENQUIRY.NAME ;* Set dictionary key as Enquiryname-language mnemonic
        IF SMC = 1 THEN  ;* when English
            R.DICT = R.DICT.HEADER  ;* Include header information
        END
        R.DICT<EB.DIC.SHORT.DESC> = R.ENQUIRY<ENQ.SHORT.DESC,SMC> ;* Add Short Description
        FOR DESC.NO = 1 TO NO.OF.DESC
            R.DICT<EB.DIC.DESCRIPTION,DESC.NO>  = R.ENQUIRY<ENQ.DESCRIPT,DESC.NO,SMC>  ;* Set Description from enquiry
        NEXT DESC.NO
        GOSUB WRITE.DICTIONARY  ;* Write information into Dictionary
    NEXT SMC


    NO.OF.LANG.SD = DCOUNT(R.ENQUIRY<ENQ.SHORT.DESC>,VM) ;* no of languages based on Short Description
    FOR SMC = 1 TO NO.OF.LANG.SD  ;* For each language
        LANG.MNE = T.LANGUAGE<SMC> ;* get the language mnemonic
        R.DICT = '' ;* Initialise Dictionary record
        DICT.KEY = ENQUIRY.NAME     ;* set Dictionary key as enquiryname-language mnemonic
        IF NOT(SMC MATCHES ENQ.LANG.CODE) THEN ;* Short Description which are not included during description mapping
            IF SMC = 1 AND R.DICT.HEADER THEN  ;* For English language and when Header exists
                R.DICT = R.DICT.HEADER        ;* Add header information
            END
            R.DICT<EB.DIC.SHORT.DESC> = R.ENQUIRY<ENQ.SHORT.DESC,SMC> ; * Set Short description
            GOSUB WRITE.DICTIONARY    ;* Write information in to dictionary
        END
    NEXT SMC

    R.DICT = '' ;* Initialise Dictionary record
    IF NOT(NO.OF.LANG.DESC) AND NOT(NO.OF.LANG.SD) AND R.DICT.HEADER THEN  ;* when Header is not added during description/short Description
        LANG.MNE = T.LANGUAGE<1>  ;* Write Header details in English
        R.DICT = R.DICT.HEADER  ;* Add header information
        DICT.KEY = ENQUIRY.NAME ;* Dictionary key will be enquiry name
        GOSUB WRITE.DICTIONARY    ;*Write header information in to enquiry
    END

    RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= FORM.HEADER>
FORM.HEADER:
*** <desc>Form Header information from Enquiry</desc>

    NO.OF.HEADERS = DCOUNT(R.ENQUIRY<ENQ.HEADER>,VM) ;* No of Header lines
    FOR HDR.VM = 1 TO NO.OF.HEADERS
        NO.OF.HDR = DCOUNT(R.ENQUIRY<ENQ.HEADER,HDR.VM>,SM) ;* get the subvalues
        FOR HDR.SM = 1 TO NO.OF.HDR
            R.DICT.HEADER<EB.DIC.ENQ.HEADER,HDR.VM,HDR.SM> = R.ENQUIRY<ENQ.HEADER,HDR.VM,HDR.SM> ;* Set Dictionary record
        NEXT HDR.SM
    NEXT HDR.VM

    RETURN
*** </region>
*-----------------------------------------------------------------------------
PROGRAM.ABORT:
    RETURN TO PROGRAM.ABORT
*-----------------------------------------------------------------------------
    END
