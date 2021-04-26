# T24TransactTranslation

T24TransactTranslation is an Excel workbook containing macros in VBA, that connects to T24 Transact Banking Software to facilitate translating the Banking Software to a different language.

## Installation
Extract the files. T24TranslationTool.xlsx is the tool, the documentation is in the form of a Word document, TranslationTool_V1_3_1.docx, but that is no longer updated. The latest version of the documentation is in the [__wiki for this repository__](https://github.com/willemgorter/T24TransactTranslation/wiki)

It connects to T24 using HTTP-GET (up to R16) or HTTP-POST (R17 and later) to perfom OFS requests.
You need to be able to create a jboss-user in T24.
It needs a reference to Microsofts MSXML2 library.

```VBA
Set myRequest = New MSXML2.XMLHTTP60
myRequest.Open "POST", gsURL_R18, False, gsJBossUser, gsJBossPwd
myRequest.setRequestHeader "Content-Type", "application/x-www-form-urlencoded"
On Error GoTo no_http_send
myRequest.send ("ofsRequest=" & sOFS)
```

## Usage

The tool will extract translatable elements from a list of Versions, Enquiries, Tables, Lookup Lists and other elements.
The translatable phrases extracted can be automatically translated with Google Translate or Microsoft Translator as well as manually, and the translations uploaded to your T24 environment(s), and packaged.
The following screen elements can be automatically extracted and translated with the tool:
* Application-labels
* Version
* Enquiry
* Application-language fields
* Dynamic Text
* Menus
* Tabbed screens
* Composite screens
* Static Text


## Screenshots
*Extraction request screen:*
![Extraction request screen](/images/ExtractionRequestScreen.jpg)

*Translation screen:*
![Translation screen](/images/TranslationScreen.jpg)

## Contributing
For issues, please open an issue or request.

## License
None
