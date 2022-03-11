module MyModule

import Language.JSON

import public B2T2

import public Idris2JupyterVega.VegaLite

%default total

export
myBarChart : String -> String -> List (String, Double) -> VegaLite
myBarChart xName yName vals =
    TopLevelSpec_0 $ MkTopLevelUnitSpec
        {Schema = Just "https://vega.github.io/schema/vega-lite/v5.json"}
        (Data_0 $ Data_0 $ DataSource_1 $ MkInlineData $ InlineDataset_3 $ map (\(name, val) => JObject [(xName, JString name), (yName, JNumber val)]) vals)
        {encoding = Just $ MkFacetedEncoding
            {x = Just $ PositionDef_0 $ MkPositionFieldDef
                {field = Just $ Field_0 xName}
                {type = Just StandardTypeNominal}
            }
            {y = Just $ PositionDef_0 $ MkPositionFieldDef
                {field = Just $ Field_0 yName}
                {type = Just StandardTypeQuantitative}
            }
        }
        (AnyMark_2 MarkBar)

export
pHackingBar : {schema : Schema}
           -> (0 _ : AllColumns schema Bool)
           => {baseCol : String}
           -> Field schema baseCol Bool
           -> Table schema
           -> VegaLite
pHackingBar baseFld tbl = do
    let vals = do
        (name ** fld) <- allColumns schema
        if name == baseCol
            then [<]
            else pure (name, fisherTest baseFld fld tbl)
    myBarChart "Jelly bean" "Acne p value" (cast vals)
