module MyModule

import Language.JSON

import ODF

import public Idris2JupyterVega.VegaLite

export
myBarChart : String -> Vect n String -> Vect n Double -> VegaLite
myBarChart description names xs =
    let vals = toList $ zip names xs in
    TopLevelSpec_0 $ MkTopLevelUnitSpec
        {Schema = Just "https://vega.github.io/schema/vega-lite/v5.json"}
        (Data_0 $ Data_0 $ DataSource_1 $ MkInlineData $ InlineDataset_3 $ map (\(name, x) => JObject [("a", JString name), ("b", JNumber x)]) vals)
        {description = Just description}
        {encoding = Just $ MkFacetedEncoding
            {x = Just $ PositionDef_0 $ MkPositionFieldDef
                {axis = Just $ Axis_0 $ MkAxis {labelAngle = Just $ LabelAngle_0 0}}
                {field = Just $ Field_0 "a"}
                {type = Just StandardTypeNominal}
            }
            {y = Just $ PositionDef_0 $ MkPositionFieldDef
                {field = Just $ Field_0 "b"}
                {type = Just StandardTypeQuantitative}
            }
        }
        (AnyMark_2 MarkBar)

export
parseSomeData : HasIO io => (fileName : String) -> io (Maybe $ List (String, Double))
parseSomeData fileName = do
    Right ods <- readODS fileName
        | Left err => pure Nothing
    pure $ do
        sheet <- findSheet "Sheet1" ods
        let cells = slice (MkCellRange (MkCellRef 0 0) 2 9) sheet
        traverse (parseRow . map (.value)) $ toList cells
  where
    getStr : CellValue -> Maybe String
    getStr (TextCell str) = Just str
    getStr _ = Nothing

    getDouble : CellValue -> Maybe Double
    getDouble (FloatCell dbl) = Just dbl
    getDouble _ = Nothing

    parseRow : Vect 2 CellValue -> Maybe (String, Double)
    parseRow [name, x] = pure (!(getStr name), !(getDouble x))

export
transpose : (xs : List (a, b)) -> (Vect (length xs) a, Vect (length xs) b)
transpose [] = ([], [])
transpose ((x, y) :: rest) =
    let (xs, ys) = transpose rest in
    (x :: xs, y :: ys)
