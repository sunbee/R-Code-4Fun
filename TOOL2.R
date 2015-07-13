####Script for analysing copy number data
####Created by Nagabhushana Ithal on 30 April 2015


##Select your file for analysis
	message("Please Choose Your File For Analysis")
		X<-file.choose()


##Read selected file
	dataFile = read.table(X, header = TRUE, fill=TRUE, skip=10)
		


##Extract data for analysis from the input file
	col2=2
	col7=7
		d1=dataFile[c(1:384), c(col2, col7)]
		d2=dataFile[c(389:772), c(col2, col7)]


		colnames(d1) <- c("Sample", "VIC")
		colnames(d2) <- c("Sample", "FAM")

		d<-merge(d1, d2, c("Sample"))


		d$VIC <- as.numeric(as.character(d$VIC)) 
		d$FAM <- as.numeric(as.character(d$FAM))

##Set limit of detection
	d$VIC[d$VIC >= 34] <- 0

##Calculate delta-ct

	d$dCT=d$VIC-d$FAM


##Convert 384 well format to 96 well format
row.names (d)<-c(1, 17, 6, 18, 7, 19, 8, 20, 9, 21, 10, 13, 22, 11, 23, 12, 24, 2, 14, 3, 15, 4, 16, 5, 25, 41, 30, 42, 31, 43, 32, 44, 33, 45, 34, 37, 46, 35, 47, 36, 48, 26, 38, 27, 39, 28, 40, 29, 49, 65, 54, 66, 55, 67, 56, 68, 57, 69, 58, 61, 70, 59, 71, 60, 72, 50, 62, 51, 63, 52, 64, 53, 73, 89, 78, 90, 79, 91, 80, 92, 81, 93, 82, 85, 94, 83, 95, 84, 96, 74, 86, 75, 87, 76, 88, 77, 97, 113, 102, 114, 103, 115, 104, 116, 105, 117, 106, 109, 118, 107, 119, 108, 120, 98, 110, 99, 111, 100, 112, 101, 121, 137, 126, 138, 127, 139, 128, 140, 129, 141, 130, 133, 142, 131, 143, 132, 144, 122, 134, 123, 135, 124, 136, 125, 145, 161, 150, 162, 151, 163, 152, 164, 153, 165, 154, 157, 166, 155, 167, 156, 168, 146, 158, 147, 159, 148, 160, 149, 169, 185, 174, 186, 175, 187, 176, 188, 177, 189, 178, 181, 190, 179, 191, 180, 192, 170, 182, 171, 183, 172, 184, 173, 193, 209, 198, 210, 199, 211, 200, 212, 201, 213, 202, 205, 214, 203, 215, 204, 216, 194, 206, 195, 207, 196, 208, 197, 217, 233, 222, 234, 223, 235, 224, 236, 225, 237, 226, 229, 238, 227, 239, 228, 240, 218, 230, 219, 231, 220, 232, 221, 241, 257, 246, 258, 247, 259, 248, 260, 249, 261, 250, 253, 262, 251, 263, 252, 264, 242, 254, 243, 255, 244, 256, 245, 265, 281, 270, 282, 271, 283, 272, 284, 273, 285, 274, 277, 286, 275, 287, 276, 288, 266, 278, 267, 279, 268, 280, 269, 289, 305, 294, 306, 295, 307, 296, 308, 297, 309, 298, 301, 310, 299, 311, 300, 312, 290, 302, 291, 303, 292, 304, 293, 313, 329, 318, 330, 319, 331, 320, 332, 321, 333, 322, 325, 334, 323, 335, 324, 336, 314, 326, 315, 327, 316, 328, 317, 337, 353, 342, 354, 343, 355, 344, 356, 345, 357, 346, 349, 358, 347, 359, 348, 360, 338, 350, 339, 351, 340, 352, 341, 361, 377, 366, 378, 367, 379, 368, 380, 369, 381, 370, 373, 382, 371, 383, 372, 384, 362, 374, 363, 375, 364, 376, 365)

##Re order rows to match 96 well format
	d<-d[order(as.numeric(rownames(d))),,drop=FALSE]

##Create copy call column
	d$CALL<-d$dCT

##Copy call based on delta-Ct values
	d$CALL[d$CALL >= 1.750001] <- 4
	d$CALL[d$CALL == 0] <- 5
	d$CALL[d$CALL <= -1.250000] <- 5
	d$CALL[d$CALL <= 0.250000] <- 6
	d$CALL[d$CALL <= 1.750000] <- 7
	d$CALL[d$CALL == 5] <- 0
	d$CALL[d$CALL == 6] <- 1
	d$CALL[d$CALL == 7] <- 2


##Plot control samples for data QC
	Control<-d[c(85:96),]
	plot(Control$Sample, Control$CALL, pch = 2, col=2, xlab="WELL ID", ylab="COPY CALL", main="LH244 CONTROL CALL", xlim = c(76, 100))

##Save file as csv at user defined directory
	require(tcltk)
	write.csv(d,file = tclvalue(tcl("tk_getSaveFile")))