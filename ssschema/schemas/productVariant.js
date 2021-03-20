export default {
  title: 'Product variant',
  name: 'productVariant',
  type: 'object',
  fields: [
    {
      title: 'Título',
      name: 'title',
      type: 'string',
    },
    {
      title: "Descripción",
      name: "description",
      type: "string",
    },
    {
      title: 'Variante',
      name: 'variant',
      type: 'number',
    },
    {
      title: 'Precio',
      name: 'price',
      type: 'number',
    },
    {
      title: 'SKU',
      name: 'sku',
      type: 'string',
    },
    {
      title: 'Tipo',
      name: 'tags',
      type: 'array',
      of: [
        {
          type: 'string',
        },
      ],
      options: {
        layout: 'tags',
      },
    },
  ],
}
