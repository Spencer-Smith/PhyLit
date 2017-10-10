checkDependencies = function() {
	if(!("ape" %in% installed.packages())){
		install.packages("ape")
	}

	library(ape)
}

uniqueAppend = function(vec, val) {
	if(val %in% vec)
		return(vec)
	return(append(vec, val))
}

offset = function(val, vec) {
	offset = 0
	for(item in vec) {
		if (val > item)
			offset = offset + 1
	}
	return(val - offset)
}

matrixMean = function(matrix) {
	nonzero = 0
	for (col in 1:ncol(matrix)) {
		for (row in 1:nrow(matrix)) {
			if (matrix[row,col] != 0) {
				nonzero = nonzero + 1
			}
		}
	}
	return(sum(matrix)/nonzero)
}

mergeTitles = function(titles, indices) {
	merged = paste(titles[indices], collapse=",")
	merged = paste("(",merged,")",sep="")
	remove = titles[indices[2:length(indices)]]
	mergedTitles = titles[!titles %in% remove]
	mergedTitles[indices[1]] = merged
	return(mergedTitles)
}

averageMatrix = function(matrix, vec) {
	newMatrix = matrix(0, nrow=nrow(matrix) - (length(vec) - 1),
		ncol=ncol(matrix) - (length(vec) - 1))
	combine = vec[1]
	remove = vec[2:length(vec)]
	for (col in 2:ncol(matrix)) {
		for (row in 1:(col-1)) {
			if ((row %in% remove) || (col %in% remove)){
				next
			}
			else if (row == combine) {
				newCol = offset(col, remove)
				sum = matrix[row,col]
				for(r in remove) {
					sum = sum + matrix[min(r,col), max(r,col)]
				}
				newMatrix[row,newCol] = sum/length(vec)
			}
			else if (col == combine){
				newRow = offset(row, remove)
				sum = matrix[row,col]
				for(c in remove) {
					sum = sum + matrix[min(c,row), max(c,row)]
				}
				newMatrix[newRow,col] = sum/length(vec)
			}
			else {
				newRow = offset(row, remove)
				newCol = offset(col, remove)
				newMatrix[newRow, newCol] = matrix[row,col]
			}
		}
	}
	return(newMatrix)
}

readData = function(path) {
	table = read.csv(path, stringsAsFactors=FALSE)
	return(table)
}

buildRelationshipMatrix = function(table) {
	rMatrix = matrix(0,nrow = nrow(table), ncol = nrow(table)) 
	for (i in 1:(nrow(table) - 1))
		for (j in (i+1):nrow(table))
			for (k in 2:ncol(table))
				if (table[i,k] == table[j,k])
					rMatrix[i,j] = rMatrix[i,j] + 1
					#May adjust to add weighted value
      return(rMatrix)
}

makeTreeText = function(matrix, table) {
      text = ""
	titles = table[,1]
	while(length(titles) > 1) {
		highVal = 0
		highVec = c()
		for (col in 2:ncol(matrix)) {
			for (row in 1:(col-1)) {
				if(matrix[row,col] > highVal) {
					highVal = matrix[row,col]
					highVec = c(row,col)
				}
				else if(matrix[row,col] == highVal &&
				(row %in% highVec || col %in% highVec)) {
					highVec = uniqueAppend(highVec, row)
					highVec = uniqueAppend(highVec, col)
				}
			}
		}
		titles = mergeTitles(titles, highVec)
		matrix = averageMatrix(matrix, highVec)
	}
	text = titles[1]
	text = paste(text,";",sep="")
	return(text)
}

plotTree = function(path) {
	checkDependencies()
	table = readData(path)
	matrix = buildRelationshipMatrix(table)
	phylogenyText = makeTreeText(matrix,table)
	tree = read.tree(text=phylogenyText)
	plot(tree)
}
