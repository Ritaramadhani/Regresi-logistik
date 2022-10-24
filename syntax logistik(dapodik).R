attach(dapodik)
aa<-dapodik
dim(aa)
#membuat kelas baru
#skor quality lebih dari 6 dikelaskan menjadi kelas=1,selainnya dikelaskan menjadi kelas=0
akreditasi<-as.factor(ifelse(aa$`peringkat akreditasi`=="A",1,0))
#membagi dataset menjadi dua bagian secara acak
#pemilihan amatan secara acak sbg data training
set.seed(1001)
acak<-sample(1:nrow(aa),0.8*nrow(aa))
#data training
data_training<-data.frame(aa$jumlah_ruang_kelas,aa$jumlah_guru_honor_sekolah)[acak,]
head(data_training)
colnames(data_training)<-c("jumlah ruan kelas","jumlah guru honor")
data_training
kelas_training<-akreditasi[acak]
head(kelas_training)
train<-data.frame(kelas_training,data_training)
#penggabunan Y dan X
colnames(train)<-c("kelas_training","jumlah_ruang_kelas","jumlah_guru_honor_sekolah")
#data testing
data_testing<-data.frame(aa$jumlah_ruang_kelas,aa$jumlah_guru_honor_sekolah)[-acak,]
colnames(data_testing)<-c("jumlah ruang kelas","jumlah guru honor")
kelas_testing <- akreditasi[-acak]
head(data_testing)
head(kelas_testing)
test <- data.frame(kelas_testing, data_testing) #penggabungan Y dgn X
colnames(test)<-c("kelas_training","jumlah_ruang_kelas","jumlah_guru_honor_sekolah")
reg1 <- glm(kelas_training ~jumlah_ruang_kelas+jumlah_guru_honor_sekolah, data = train, family = 'binomial' )
pred_1 <- predict(reg1, subset(test, select=c(2:3)), type = 'response')
hasil <- (cbind(test[,1], pred_1, ifelse(pred_1 >= 0.5, '1', '0')))

cm <- table(hasil[,3], test[,1])
akurasi <- (cm[1] + cm[4])/sum(cm)
sens <- cm[4] / (cm[2] + cm[4])
spes <- cm[1] / (cm[1] + cm[3])
print(list(akurasi,sens,spes))

