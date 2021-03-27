*-----------------------------------------------------------------------------
* <Rating>187</Rating>
*-----------------------------------------------------------------------------
      SUBROUTINE E.MENU.EXPORT(idList)

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.HELPTEXT.MENU
$INSERT I_F.HELPTEXT.MAINMENU

      FN.HELPTEXT.MENU = 'F.HELPTEXT.MENU'
      F.HELPTEXT.MENU = ''
      CALL OPF(FN.HELPTEXT.MENU, F.HELPTEXT.MENU)

      FN.HELPTEXT.MAINMENU = 'F.HELPTEXT.MAINMENU'
      F.HELPTEXT.MAINMENU = ''
      CALL OPF(FN.HELPTEXT.MAINMENU, F.HELPTEXT.MAINMENU)

      LOCATE "MENU.ID" IN D.FIELDS<1> SETTING pos ELSE pos = ''
      IF D.LOGICAL.OPERANDS<pos> <> '1' THEN RETURN      ; * EQ
      menuId    = D.RANGE.AND.VALUE<pos,1>
      menuIDs<1> = menuId
      iMainMenu = 0
      operation = ''

      CALL S.ALL.MENU.DETAILS(operation, menuId)

      idList = ''
      level = 1
      order<1> = 1
      CALL CACHE.READ(FN.HELPTEXT.MENU, menuIDs<1>, R.HELPTEXT.MENU, ERR)
     IF ERR EQ '' THEN
          xDescs<1> = R.HELPTEXT.MENU<EB.MEN.DESCRIPT>
     END ELSE
         CALL CACHE.READ(FN.HELPTEXT.MAINMENU, menuIDs<1>, R.HELPTEXT.MAINMENU, ERR)
         xDescs<1> = R.HELPTEXT.MAINMENU<EB.MME.DESCRIPT>
         iMainMenu = 1 
     END
      types = menuId<3>
      descs = menuId<2>
      actions = menuId<4>
      LOOP
         REMOVE type FROM types SETTING dummy
         REMOVE desc FROM descs SETTING dummy
         REMOVE action FROM actions SETTING dummy
      WHILE type : desc : action
         IF type EQ 4 THEN
            order<level> = 0
            menuIDs<level> = ''
            idList<-1> = type : '*' : desc : '*' : action : '*' : level : '*' : order<level> : '*' 
         END ELSE
            mytype = type
            IF iMainMenu EQ 1 THEN 
                IF level EQ 1 THEN
                    mytype = 1
                END
            END
            idList<-1> = mytype : '*' : desc : '*' : action : '*' : level : '*' : order<level> : '*' : menuIDs<level> : '*' : xDescs<level,order<level>,1> : '*' : xDescs<level,order<level>,2>
         END
         IF type EQ 3 THEN
            level = level + 1
            order<level> = 0
            menuIDs<level> = action
            CALL CACHE.READ(FN.HELPTEXT.MENU, menuIDs<level>, R.HELPTEXT.MENU, ERR)
            xDescs<level> = R.HELPTEXT.MENU<EB.MEN.DESCRIPT>
         END
         IF type EQ 4 THEN
            level = level - 1
         END
         order<level> = order<level> + 1
      REPEAT

      RETURN
   END
