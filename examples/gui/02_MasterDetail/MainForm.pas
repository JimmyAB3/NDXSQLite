{===============================================================================
  NDXSQLite GUI Example - Master/Detail Relationship
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  Demonstrates:
  - Two related TNDXSQLiteDataSet instances (master and detail)
  - Automatic detail refresh when master record changes
  - Foreign key relationship between tables
  - DBGrid display for both master and detail data
  - Cross-platform GUI (Windows, Linux, macOS)

  This example implements a classic master/detail pattern where selecting a
  category (master) automatically displays its associated products (detail).
  Key concepts demonstrated:
  - Shared database connection between multiple datasets
  - Parameterized queries for filtering detail records
  - OnNewRecord event for automatic foreign key assignment
  - OnDataChange event for synchronized navigation
===============================================================================}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, DBCtrls,
  StdCtrls, ExtCtrls, ComCtrls, DB,
  ndxsqlitedataset, ndxsqlitedatabase;

type
  { TfrmMain }
  TfrmMain = class(TForm)
    DataSourceMaster: TDataSource;
    DataSourceDetail: TDataSource;
    DBGridMaster: TDBGrid;
    DBGridDetail: TDBGrid;
    DBNavigatorMaster: TDBNavigator;
    DBNavigatorDetail: TDBNavigator;
    pnlTop: TPanel;
    pnlStatus: TPanel;
    Splitter1: TSplitter;
    pnlMaster: TPanel;
    pnlDetail: TPanel;
    lblMaster: TLabel;
    lblDetail: TLabel;
    btnRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure DataSourceMasterDataChange(Sender: TObject; Field: TField);
  private
    FDatabase: TNDXSQLiteDatabase;
    FMasterDataSet: TNDXSQLiteDataSet;
    FDetailDataSet: TNDXSQLiteDataSet;
    { Initializes database, creates tables, and loads sample data. }
    procedure InitializeDatabase;
    { Refreshes the detail dataset based on the current master record. }
    procedure RefreshDetailDataSet;
    { Updates the status bar with record counts. }
    procedure UpdateStatusBar;
    { Configures the master grid columns for category display. }
    procedure ConfigureMasterGrid;
    { Configures the detail grid columns for product display. }
    procedure ConfigureDetailGrid;
    { Event handler called when inserting a new detail record.
      Automatically sets the foreign key to the current master category. }
    procedure DetailDataSetNewRecord(DataSet: TDataSet);
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // Create a shared database connection for both datasets
  // This ensures both datasets operate within the same database context
  // and can participate in the same transactions if needed
  FDatabase := TNDXSQLiteDatabase.Create;

  // Create master dataset for categories
  FMasterDataSet := TNDXSQLiteDataSet.Create(Self);

  // Create detail dataset for products linked to categories
  FDetailDataSet := TNDXSQLiteDataSet.Create(Self);

  // Initialize database schema and sample data BEFORE connecting to DataSources
  // This prevents DataChange events from firing during initialization
  // when the database isn't ready yet
  InitializeDatabase;

  // Connect datasets to their respective data sources
  // DataSource acts as intermediary between dataset and visual controls
  DataSourceMaster.DataSet := FMasterDataSet;
  DataSourceDetail.DataSet := FDetailDataSet;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Close datasets in reverse order of dependency (detail first, then master)
  if Assigned(FDetailDataSet) then
    FDetailDataSet.Close;
  if Assigned(FMasterDataSet) then
    FMasterDataSet.Close;

  // Close and free the shared database connection
  if Assigned(FDatabase) then
  begin
    FDatabase.Close;
    FDatabase.Free;
  end;
end;

procedure TfrmMain.InitializeDatabase;
var
  DBPath: string;
begin
  // Place database in executable directory for portability
  DBPath := ExtractFilePath(ParamStr(0)) + 'masterdetail_demo.db';

  // Open the database file (creates if not exists)
  FDatabase.Open(DBPath);

  // Enable SQLite foreign key enforcement
  // This ensures referential integrity between categories and products
  FDatabase.Execute('PRAGMA foreign_keys = ON');

  // Create the master table (categories)
  // Uses UNIQUE constraint on name to prevent duplicate categories
  FDatabase.Execute(
    'CREATE TABLE IF NOT EXISTS categories (' +
    'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'name TEXT NOT NULL UNIQUE, ' +
    'description TEXT' +
    ')'
  );

  // Create the detail table (products) with foreign key constraint
  // ON DELETE CASCADE ensures products are deleted when their category is removed
  FDatabase.Execute(
    'CREATE TABLE IF NOT EXISTS products (' +
    'id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    'category_id INTEGER NOT NULL, ' +
    'name TEXT NOT NULL, ' +
    'price REAL DEFAULT 0.0, ' +
    'stock INTEGER DEFAULT 0, ' +
    'FOREIGN KEY (category_id) REFERENCES categories(id) ON DELETE CASCADE' +
    ')'
  );

  // Populate with sample data if tables are empty
  if FDatabase.ExecuteScalar('SELECT COUNT(*) FROM categories') = 0 then
  begin
    // Use explicit transaction for atomic insertion of related data
    FDatabase.BeginTransaction;
    try
      // Insert sample categories
      FDatabase.Execute('INSERT INTO categories (name, description) VALUES ' +
        '(''Electronics'', ''Electronic devices and accessories'')');
      FDatabase.Execute('INSERT INTO categories (name, description) VALUES ' +
        '(''Books'', ''Physical and digital books'')');
      FDatabase.Execute('INSERT INTO categories (name, description) VALUES ' +
        '(''Clothing'', ''Apparel and fashion items'')');
      FDatabase.Execute('INSERT INTO categories (name, description) VALUES ' +
        '(''Home & Garden'', ''Home improvement and gardening supplies'')');

      // Insert sample products for Electronics (category_id = 1)
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(1, ''Smartphone X12'', 699.99, 50)');
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(1, ''Wireless Headphones'', 149.99, 120)');
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(1, ''USB-C Cable'', 12.99, 500)');

      // Insert sample products for Books (category_id = 2)
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(2, ''Programming Lazarus'', 49.99, 30)');
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(2, ''Database Design'', 39.99, 25)');

      // Insert sample products for Clothing (category_id = 3)
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(3, ''Cotton T-Shirt'', 19.99, 200)');
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(3, ''Denim Jeans'', 59.99, 80)');
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(3, ''Winter Jacket'', 129.99, 40)');

      // Insert sample products for Home & Garden (category_id = 4)
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(4, ''Garden Hose 50ft'', 29.99, 60)');
      FDatabase.Execute('INSERT INTO products (category_id, name, price, stock) VALUES ' +
        '(4, ''LED Light Bulb 4-Pack'', 14.99, 150)');

      FDatabase.Commit;
    except
      // Rollback on any error to maintain data consistency
      FDatabase.Rollback;
      raise;
    end;
  end;

  // Configure master dataset to use the shared database connection
  FMasterDataSet.Database := FDatabase;
  FMasterDataSet.TableName := 'categories';
  FMasterDataSet.PrimaryKey := 'id';
  FMasterDataSet.SQL.Text := 'SELECT id, name, description FROM categories ORDER BY name';
  FMasterDataSet.Open;

  // Configure detail dataset with shared connection
  // Note: SQL is set dynamically in RefreshDetailDataSet based on selected category
  FDetailDataSet.Database := FDatabase;
  FDetailDataSet.TableName := 'products';
  FDetailDataSet.PrimaryKey := 'id';
  // Assign event handler for automatic foreign key assignment on new records
  FDetailDataSet.OnNewRecord := @DetailDataSetNewRecord;

  // Configure visual appearance of both grids
  ConfigureMasterGrid;
  ConfigureDetailGrid;

  // Load detail records for the initially selected category
  RefreshDetailDataSet;

  // Display database location in status bar
  pnlStatus.Caption := 'Database: ' + DBPath;
end;

procedure TfrmMain.ConfigureMasterGrid;
begin
  // Define columns for the category (master) grid
  DBGridMaster.Columns.Clear;
  with DBGridMaster.Columns.Add do
  begin
    FieldName := 'id';
    Title.Caption := 'ID';
    Width := 40;
    ReadOnly := True;  // Primary key is auto-generated
  end;
  with DBGridMaster.Columns.Add do
  begin
    FieldName := 'name';
    Title.Caption := 'Category Name';
    Width := 150;
  end;
  with DBGridMaster.Columns.Add do
  begin
    FieldName := 'description';
    Title.Caption := 'Description';
    Width := 250;
  end;
end;

procedure TfrmMain.ConfigureDetailGrid;
begin
  // Define columns for the product (detail) grid
  // Note: category_id is intentionally not shown since it's implicit
  // from the master/detail relationship
  DBGridDetail.Columns.Clear;
  with DBGridDetail.Columns.Add do
  begin
    FieldName := 'id';
    Title.Caption := 'ID';
    Width := 40;
    ReadOnly := True;  // Primary key is auto-generated
  end;
  with DBGridDetail.Columns.Add do
  begin
    FieldName := 'name';
    Title.Caption := 'Product Name';
    Width := 180;
  end;
  with DBGridDetail.Columns.Add do
  begin
    FieldName := 'price';
    Title.Caption := 'Price';
    Width := 80;
  end;
  with DBGridDetail.Columns.Add do
  begin
    FieldName := 'stock';
    Title.Caption := 'Stock';
    Width := 60;
  end;
end;

procedure TfrmMain.RefreshDetailDataSet;
var
  CategoryId: Integer;
begin
  // Close the detail dataset before changing its query
  FDetailDataSet.Close;

  // Only load detail records if master has a valid current record
  if FMasterDataSet.Active and not FMasterDataSet.IsEmpty then
  begin
    // Get the primary key of the currently selected category
    CategoryId := FMasterDataSet.FieldByName('id').AsInteger;

    // Set up a parameterized query to filter products by category
    // Using parameters prevents SQL injection and improves performance
    FDetailDataSet.SQL.Text :=
      'SELECT id, category_id, name, price, stock FROM products ' +
      'WHERE category_id = :cat_id ORDER BY name';

    // Bind the parameter value before opening
    FDetailDataSet.Params.ParamByName('cat_id').AsInteger := CategoryId;

    // Open the filtered detail dataset
    FDetailDataSet.Open;

    // Update label to show current category context and product count
    lblDetail.Caption := Format('Products in "%s" (%d items)',
      [FMasterDataSet.FieldByName('name').AsString, FDetailDataSet.RecordCount]);
  end
  else
  begin
    // No category selected - display informative message
    lblDetail.Caption := 'Products (no category selected)';
  end;

  UpdateStatusBar;
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
begin
  // Reload both datasets from the database
  // This discards any uncommitted changes and refreshes from persistent storage
  FMasterDataSet.Close;
  FMasterDataSet.Open;
  RefreshDetailDataSet;
end;

procedure TfrmMain.DataSourceMasterDataChange(Sender: TObject; Field: TField);
begin
  // This event fires whenever the master dataset's current record changes
  // Field = nil indicates a record navigation (not a field edit)
  // We also verify the database is ready to avoid events during initialization
  if (Field = nil) and Assigned(FDatabase) and FDatabase.IsOpen then
    RefreshDetailDataSet;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  // Display summary counts for both master and detail datasets
  if FMasterDataSet.Active then
    pnlStatus.Caption := Format('Categories: %d | Products: %d',
      [FMasterDataSet.RecordCount, FDetailDataSet.RecordCount])
  else
    pnlStatus.Caption := 'Dataset closed';
end;

procedure TfrmMain.DetailDataSetNewRecord(DataSet: TDataSet);
var
  CategoryField: TField;
  MasterIdField: TField;
begin
  // This event handler is called when inserting a new product record
  // It automatically sets the foreign key (category_id) to the currently
  // selected category, ensuring referential integrity

  // Safety checks to prevent errors if master isn't properly configured
  if not Assigned(FMasterDataSet) then
    Exit;
  if not FMasterDataSet.Active then
    Exit;
  if FMasterDataSet.IsEmpty then
    Exit;

  // Find the foreign key field in the detail record
  CategoryField := DataSet.FindField('category_id');
  // Find the primary key field in the master record
  MasterIdField := FMasterDataSet.FindField('id');

  // Assign the master's ID to the detail's foreign key
  if Assigned(CategoryField) and Assigned(MasterIdField) then
    CategoryField.AsInteger := MasterIdField.AsInteger;
end;

end.
