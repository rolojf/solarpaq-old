module TemplateType exposing (TemplateType(..))

import TemplateMetadata


type TemplateType
    = BlogPost TemplateMetadata.BlogPost
    | Page TemplateMetadata.Page
    | BlogIndex TemplateMetadata.BlogIndex
    | SelComp1 TemplateMetadata.SelComp1

