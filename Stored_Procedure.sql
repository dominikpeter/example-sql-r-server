/*

@TransactionTable = Table with 3 columns in the form of TransactionID, Item and Measure
@Source = Key to identify
@Algorithm = apriori oder eclat (eclat is slower but mor scalable)
@Support = a numeric value for the minimal support of an item set (default: 0.00001)
@Confidence = a numeric value for the minimal confidence of rules/association hyperedges (default: 0.5)

Example:

exec dbo.spMarketBasket @TransactionTable = N'select OrderNo, idItem, Quantity from transactions;',
						@Source = 'ERP',
						@Algorithm = 'apriori',
						@Support = 0.0001,
						@Confidence = 0.5


exec dbo.spMarketBasket @TransactionTable = N'select OrderNo, idItem, Quantity from online_transactions;',
						@Source = 'Online',
						@Algorithm = 'eclat',
						@Support = 0.000001,
						@Confidence = 0.5

*/


CREATE PROCEDURE [dbo].[spMarketBasket]
	@TransactionTable nvarchar(max) = N'select OrderNo, I.idItem, Quantity from infopool.fact.v_sales',
	@Source varchar(500) = 'ERP',
	@Algorithm varchar(500) = 'apriori',
	@Support float = 0.00001,
	@Confidence float = 0.5
AS


SET @TransactionTable = isnull(@TransactionTable, 'select OrderNo, I.idItem, Quantity from transactions')
SET @Source = isnull(@Source, 'ERP)
SET @Algorithm = lower(isnull(@Algorithm, 'apriori'))
SET @Support = isnull(@Support, 0.00001)
SET @Confidence = isnull(@Confidence, 0.5) 

DECLARE @SupportString varchar(500) = cast(@Support as varchar(500))
DECLARE @ConfidenceString varchar(500) = cast(@Confidence as varchar(500))

DECLARE @rscript nvarchar(max)
DECLARE @variables nvarchar(max)


SET @variables = N'
	Source		= "'+@Source+'"
	Algorithm	= "'+@Algorithm+'"
	Support		= ' +@SupportString+'
	Confidence	= ' +@ConfidenceString+'
'

SET @rscript = N'

	library(magrittr)
	library(data.table)
	library(Matrix)
	library(arules)
	library(stringr)

	clean_rule <- function(x){
	  x %>% str_replace_all("\\{|\\}", "")
	}

	colnames(DT) <- c("TransactionID", "ID", "Measure")
	DT <- as.data.table(DT)

	DT <- DT[, `:=` (TransactionID = TransactionID %>% as.factor,
                     ID = ID %>% as.factor,
                     Measure = ifelse(is.na(Measure), 0.0, Measure))] %>% 
		  na.omit()

	g <- sparseMatrix(i = DT$ID %>% as.integer(),
					  j = DT$TransactionID %>% as.integer(),
					  x = DT$Measure)

	colnames(g) = levels(DT$TransactionID)
	rownames(g) = levels(DT$ID)

	tr <- as(g, "ngCMatrix") %>% as("transactions")
	g <- NULL

	if (Algorithm == "apriori"){

		itemsets <- tr %>%
			apriori(parameter = list(target = "rules",
						support=Support,
						confidence=Confidence,
						minlen = 2,
						maxlen=2))
		quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = itemsets)
	
	} else if (Algorithm == "eclat"){

		itemsets <- tr %>%
			eclat(parameter = list( support=Support,
						minlen = 2, maxlen=2)) %>%
			ruleInduction(tr, confidence = Confidence)

	} else {
		stop("No or unsupported Algorithm declared")
	}

	DT <- data.table(
	  lhs = labels(lhs(itemsets)),
	  rhs = labels(rhs(itemsets)), 
	  itemsets@quality)

	DT[,`:=` (source = Source,
	          antecedants = lhs %>% clean_rule,
              consequents = rhs %>% clean_rule)]


	DT <- DT[, .(source, antecedants, consequents, support, confidence, lift)]

	# DT %>% setkey(antecedants)
	# DT[, rank := frank(-lift, ties.method = "dense"), by = key(DT)]

	'

SET @rscript = @variables + @rscript

EXECUTE sp_execute_external_script
	@language = N'R'
	,@script = @rscript
	,@input_data_1 = @TransactionTable
	,@input_data_1_name  = N'DT'
	,@output_data_1_name =  N'DT'
	WITH RESULT SETS ((     [source] varchar(5000) NULL,
				[antecedants] varchar(5000) NULL,
				[consequents] varchar(5000) NULL,
				[support] float,
				[confidence] float,
				[lift] float));



GO
