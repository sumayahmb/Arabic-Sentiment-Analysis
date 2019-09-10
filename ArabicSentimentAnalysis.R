## The following script should work on Windows 
## first get and save your existing locale
print(Sys.getlocale(category = "LC_CTYPE"))
original_ctype <- Sys.getlocale(category = "LC_CTYPE")

## Switch to the appropriate locale for the script
Sys.setlocale("LC_CTYPE","en_US.UTF-8")




## Step (1)
# Select the desired text and save as text file with UTF-8 encoding



## Step (2)
# load required packages
library(tm)
library(arabicStemR)

## Step (3)
# read text with UTF-8 encoding
arabic_text<-readLines("food.txt", encoding="UTF-8")





## start cleaning

arabic_text<-removePunctuation(arabic_text)
arabic_text<-removeNumbers(arabic_text)
arabic_text<-removeNewlineChars(arabic_text)
arabic_text<-stripWhitespace(arabic_text)



# have a look !
arabic_text
arabic_text<-arabic_text[arabic_text != ""]

## Step (4) Read Sentiments

sents<-readLines("food_sent.txt", encoding="UTF-8")



sents
table(sents)

sents <- sub("\uFEFF", "", sents) 
table(sents)
arabic_text <- sub("\uFEFF", "", arabic_text) 

###############################################
## start transliteration part
mydf<-data.frame(arabic_text,sents, stringsAsFactors = F)
mydf$arabic_text_t <-transliterate(mydf$arabic_text)


library(RTextTools)
# Create the document term matrix
dtMatrix <- create_matrix(mydf["arabic_text_t"])
# Configure the training data
str(mydf)
mydf$sents<-factor(mydf$sents)
container <- create_container(dtMatrix, mydf$sents, trainSize = 1:696, virgin = F)

# train a SVM Model
model <- train_model(container, "SVM", kernel="linear", cost=1)
# new data

myterms<-c("جربت عندهم الحلا والقهوة وكانت التجربة جدا جميله❤️❤️ راجعه قريب اجرب الاكل ان شاء الله ",
           "♥️♥️♥️♥️♥️♥️♥️",
           "ازعاج مو طبيعي من جرس المطبخ،ما يحتاج ٦-٧ رنات عشان يعرف الويتر ان الطلب جاهز! كل المطعم عرف واللهوعلى كل طبق هالحالة -_- القهوة الامريكيةكويسة، كيكة الشوكلاته بالبينبتر حلوه بس طبقةالشوكلاته ماتتكسر",
           "الاسعار مبالغ فيها جدا جدا جدا  البراوني قد الكف وفوقها مكسر ايسكريم ساندوتش ابو ريال؟؟ ب حوالي ٥٠ ريال!!!",
           "الاسعار مبالغ فيها،،، هبة عالم على اي شي جديد،،، ماراح اكرر الزيارة",
           "الاكل لذيذ والقهوه توب بس الخدمه للاسف تعبانه",
           "الخدمة بطيئة جداً ، القهوة اقل من عادية  كيكة التمر عادية بس سعرها مبالغ فيه ! مستحيل ارجع له لا طعم ولا سعر ولا حتى خدمة  اي بعد حتى ما يعطون فاتورة ؟!",
           "الخدمة سيئة جداً والاسعار مبالغ فيها وقائمة المأكولات غير واضحه ولا فيها صور الشيء الجميل المكان رايق ويفتح النفس",
           "الخدمه دون المستوى  ما ازوره مره ثانيه 💁🏻",
           "الصراحة يحتاج الى تطوييييييييير المكان جميل لكن العاملين فيه يحتاجون يتدربون الاكل اقل اقل من عادي يحتاج انهم يدرسون المينيو جيداً",
           "الفته خفيفه ❤",
           "المشروبات سيئة مافيها طعم كلها حليب! لو يركزون على الطعم افضل من طريقة التقديم، الشي الوحيد الزين المكان والجلسات الخارجية",
           "المكان حلو ورايق لكن الزحمة والخدمة مخربته للأسف 💔، جربت لاتيه الزعفران مايسوى واقل من عادي  كيك البينت بيتر كانت حلوووة بس دسممة جداً 😍👌🏻❤️ أكرر التجربة بس بالنهار اجمل 🌸",
           "المكان فقط",
           "بلا مبالغه ألذذذذذذ فتة باذنجان بالرياض",
           "حلو مره الاكل والحلى خاصة كيكة العسل😍👌🏻👌🏻",
           "ذا بيرفكت براوني يفوووووووووز",
           "زحمه غير طبيعية بسبب سوء التنظيم كننا في مقصف حتى قائمه بأسماء الانتظار مافي!! الموظف يسأل كل مره مين جاي قبل😶!، غير بطئ الخدمه شي مأساوي ابتداء من طلب المنيو وانتهاء بطلب الفاتوره",
           "زرته ثلاث مرات على التوالي تطورات ملحوظة في الخدمة، ابداع في تقديم الأطباق و الأكل نظيف و ديكور المكان ساحر و حميمي، متوفر موسيقى و جلسات خارجية أعتقد أنه بيكون موقعي الشتوي ❤️❤️❤️",
           "ليترالي أسوأ خدمة ممكن تشوفونها! الموظفين كثير عرب وفلبينيين وكلهم نفس الأسلوب والنفسية التعبانة! طلبت الأكل تيك اوي ماقدرت أجلس في المكان أكثر",
           "مطعم جميل جداً ، اكل وحلا شي من الاخر")
transliterate(myterms)
predictionData <- list("jrbt 3ndhm al7la walQhw0 wkant altjrb0 jda jmylh❤️❤️ raj3h Qryb ajrb alakl an Waq allh ",
                       "♥️♥️♥️♥️♥️♥️♥️",
                       "az3aj mw Tby3y mn jrs almTbK،ma y7taj ٦-٧ rnat 3Wan y3rf alwytr an alTlb jahz! kl almT3m 3rf wallhw3lA kl TbQ hal7al0 -_- alQhw0 alamryky0kwys0، kyk0 alWwklath balbynbtr 7lwh bs TbQ0alWwklath mattksr",
                       "alas3ar mbalG fyha jda jda jda  albrawny Qd alkf wfwQha mksr ayskrym sandwtW abw ryal؟؟ b 7waly ٥٠ ryal!!!",
                       "alas3ar mbalG fyha،،، hb0 3alm 3lA ay Wy jdyd،،، mara7 akrr alzyar0",
                       "alakl liyi walQhwh twb bs alKdmh llasf t3banh",
                       "alKdm0 bTy50 jdaً ، alQhw0 aQl mn 3ady0  kyk0 altmr 3ady0 bs s3rha mbalG fyh ! mst7yl arj3 lh la T3m wla s3r wla 7tA Kdm0  ay b3d 7tA ma y3Twn fatwr0 ؟!",
                       "alKdm0 sy50 jdaً walas3ar mbalG fyha wQa5m0 almakwlat Gyr waD7h wla fyha Swr alWyq aljmyl almkan rayQ wyft7 alnfs",
                       "alKdmh dwn almstwA  ma azwrh mrh Uanyh \U0001f481\U0001f3fb",
                       "alSra70 y7taj alA tTwyyyyyyyyyr almkan jmyl lkn al3amlyn fyh y7tajwn ytdrbwn alakl aQl aQl mn 3ady y7taj anhm ydrswn almynyw jydaً",
                       "alfth Kfyfh ❤",
                       "almWrwbat sy50 mafyha T3m klha 7lyb! lw yrkzwn 3lA alT3m afDl mn TryQ0 altQdym، alWy alw7yd alzyn almkan waljlsat alKarjy0",
                       "almkan 7lw wrayQ lkn alz7m0 walKdm0 mKrbth llasf \U0001f494، jrbt latyh alz3fran mayswA waQl mn 3ady  kyk albynt bytr kant 7lwww0 bs dsmm0 jdaً \U0001f60d\U0001f44c\U0001f3fb❤️ akrr altjrb0 bs balnhar ajml \U0001f338",
                       "almkan fQT",
                       "bla mbalGh aliiiiii ft0 bainjan balryaD",
                       "7lw mrh alakl wal7lA KaS0 kyk0 al3sl\U0001f60d\U0001f44c\U0001f3fb\U0001f44c\U0001f3fb",
                       "ia byrfkt brawny yfwwwwwwwwwz",
                       "z7mh Gyr Tby3y0 bsbb swq altnZym knna fy mQSf 7tA Qa5mh basmaq alantZar mafy!! almwZf ysal kl mrh myn jay Qbl\U0001f636!، Gyr bT5 alKdmh Wy masawy abtdaq mn Tlb almnyw wanthaq bTlb alfatwrh",
                       "zrth UlaU mrat 3lA altwaly tTwrat ml7wZ0 fy alKdm0، abda3 fy tQdym alaTbaQ w alakl nZyf w dykwr almkan sa7r w 7mymy، mtwfr mwsyQA w jlsat Karjy0 a3tQd anh bykwn mwQ3y alWtwy ❤️❤️❤️",
                       "lytraly aswa Kdm0 mmkn tWwfwnha! almwZfyn kUyr 3rb wflbynyyn wklhm nfs alaslwb walnfsy0 alt3ban0! Tlbt alakl tyk awy maQdrt ajls fy almkan akUr",
                       "mT3m jmyl jdaً ، akl w7la Wy mn alaKr")
#reverse.transliterate(c(" maynf3W " ," yaryt " ,  " mmtaz "))



# create a prediction document term matrix


##If you're using RStudio and get this error, yout should to use the trace("create_matrix", edit=T) before create_matrix method. Then new window will appear with the lib code, go to line 42 and change Acronym to acronym.
##trace("create_matrix", edit=T)
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)
predSize = length(predictionData);

##the following gives error if none of test data matches with training data.


predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
# predict
results <- classify_model(predictionContainer, model)
results
final_report<-cbind(results,data=unlist(predictionData), myterms)
####################################################
write.table(x = final_report, file = "exported_mtcars.txt")






## ...and don't forget to switch back
Sys.setlocale("LC_CTYPE", original_ctype)



