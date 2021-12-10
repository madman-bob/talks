module MyModule

import Language.JSON

import ODF

import public Idris2JupyterVega.VegaLite

export
myBarChart : String -> Vect n String -> Vect n Double -> VegaLite
myBarChart description names xs =
    let vals = toList $ zip names xs in
    TopLevelSpec_0 $ MkTopLevelUnitSpec
        (Just "https://vega.github.io/schema/vega-lite/v5.json")
        Nothing Nothing Nothing Nothing Nothing Nothing
        (Data_0 $ Data_0 $ DataSource_1 $ MkInlineData Nothing Nothing $ InlineDataset_3 $ map (\(name, x) => JObject [("a", JString name), ("b", JNumber x)]) vals)
        Nothing
        (Just description)
        (Just $ MkFacetedEncoding
            Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            (Just $ PositionDef_0 $ MkPositionFieldDef
                Nothing
                (Just $ Axis_0 $ MkAxis Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just $ LabelAngle_0 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
                Nothing Nothing
                (Just $ Field_0 "a")
                Nothing Nothing Nothing Nothing Nothing Nothing
                (Just StandardTypeNominal)
            )
            Nothing Nothing Nothing
            (Just $ PositionDef_0 $ MkPositionFieldDef
                Nothing Nothing Nothing Nothing
                (Just $ Field_0 "b")
                Nothing Nothing Nothing Nothing Nothing Nothing
                (Just StandardTypeQuantitative)
            )
            Nothing Nothing Nothing
        )
        Nothing
        (AnyMark_2 MarkBar)
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
