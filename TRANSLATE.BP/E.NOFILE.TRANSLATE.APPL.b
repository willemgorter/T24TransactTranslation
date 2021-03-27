*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE E.NOFILE.TRANSLATE.APPL(Y.OUT)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*-----------------------------------------------------------------------------

    v.TDY.SelectionFields = ENQ.SELECTION<2>
    v.TDY.SelectionOperators = ENQ.SELECTION<3>
    v.TDY.SelectionValues = ENQ.SELECTION<4>
    CONVERT @VM TO @FM IN v.TDY.SelectionFields
    CONVERT @VM TO @FM IN v.TDY.SelectionOperators
    CONVERT @VM TO @FM IN v.TDY.SelectionValues

    LOCATE '@ID' IN v.TDY.SelectionFields SETTING POSN THEN
    	Value = v.TDY.SelectionValues<POSN>
    END
    LOCATE 'REC.ID' IN v.TDY.SelectionFields SETTING POSID THEN
    	ValueId = v.TDY.SelectionValues<POSID>
    	CONVERT @SM TO SPACE(1) IN ValueId
    	OperatorId = v.TDY.SelectionOperators<POSID>
    END
	RetriveParams<1> = Value
	RetriveParams<2> = OperatorId : @VM : ValueId
    CALL BLM.TRANSLATION.RETRIVE(RetriveParams,Y.OUT)
	Y.OUT = CHANGE(Y.OUT,@VM,"#")

    RETURN
    END
