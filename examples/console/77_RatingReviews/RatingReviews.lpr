{===============================================================================
  NDXSQLite Example 77 - Ratings and Reviews System
  (c) 2026 Nicolas DEOUX - NDX Software. MIT License.

  A comprehensive rating and review system demonstrating:
  - Star ratings (1-5 scale) with aggregations
  - Written reviews with titles and content
  - Review helpfulness voting
  - Verified purchase badges
  - Rating distribution histograms
  - Review moderation workflow
  - Seller/owner responses to reviews
  - Multiple sort options (recent, helpful, rating)
  - Review analytics and statistics

  Cross-platform: Windows, Linux, macOS
===============================================================================}

program RatingReviews;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, DB, DateUtils, Variants, StrUtils,
  ndxsqliteconnection;

var
  Conn: TNDXSQLiteConnection;

// =============================================================================
// Schema Creation
// =============================================================================
{ Creates the database schema with all required tables and indexes. }
procedure CreateSchema;
begin
  WriteLn('1. Creating Ratings & Reviews Schema');
  WriteLn('   ===================================');

  // Products/Items to be reviewed
  Conn.ExecuteNonQuery(
    'CREATE TABLE products (' +
    '  id INTEGER PRIMARY KEY,' +
    '  name TEXT NOT NULL,' +
    '  category TEXT,' +
    '  price REAL,' +
    '  seller TEXT,' +
    '  rating_count INTEGER DEFAULT 0,' +
    '  rating_sum INTEGER DEFAULT 0,' +
    '  rating_avg REAL DEFAULT 0.0,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Users who can write reviews
  Conn.ExecuteNonQuery(
    'CREATE TABLE users (' +
    '  id INTEGER PRIMARY KEY,' +
    '  username TEXT UNIQUE NOT NULL,' +
    '  display_name TEXT,' +
    '  is_verified_buyer INTEGER DEFAULT 0,' +
    '  total_reviews INTEGER DEFAULT 0,' +
    '  helpful_votes_received INTEGER DEFAULT 0,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Purchases (for verified purchase badge)
  Conn.ExecuteNonQuery(
    'CREATE TABLE purchases (' +
    '  id INTEGER PRIMARY KEY,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  product_id INTEGER NOT NULL REFERENCES products(id),' +
    '  purchase_date TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(user_id, product_id)' +
    ')');

  // Reviews
  Conn.ExecuteNonQuery(
    'CREATE TABLE reviews (' +
    '  id INTEGER PRIMARY KEY,' +
    '  product_id INTEGER NOT NULL REFERENCES products(id) ON DELETE CASCADE,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  rating INTEGER NOT NULL CHECK(rating >= 1 AND rating <= 5),' +
    '  title TEXT,' +
    '  content TEXT,' +
    '  pros TEXT,' +
    '  cons TEXT,' +
    '  is_verified_purchase INTEGER DEFAULT 0,' +
    '  status TEXT DEFAULT ''approved'' CHECK(status IN (''pending'', ''approved'', ''rejected'', ''flagged'')),' +
    '  helpful_count INTEGER DEFAULT 0,' +
    '  not_helpful_count INTEGER DEFAULT 0,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  updated_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(product_id, user_id)' +
    ')');

  // Review helpfulness votes
  Conn.ExecuteNonQuery(
    'CREATE TABLE review_votes (' +
    '  id INTEGER PRIMARY KEY,' +
    '  review_id INTEGER NOT NULL REFERENCES reviews(id) ON DELETE CASCADE,' +
    '  user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  is_helpful INTEGER NOT NULL,' +
    '  created_at TEXT DEFAULT (datetime(''now'')),' +
    '  UNIQUE(review_id, user_id)' +
    ')');

  // Review images
  Conn.ExecuteNonQuery(
    'CREATE TABLE review_images (' +
    '  id INTEGER PRIMARY KEY,' +
    '  review_id INTEGER NOT NULL REFERENCES reviews(id) ON DELETE CASCADE,' +
    '  image_url TEXT NOT NULL,' +
    '  caption TEXT,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Seller/Owner responses
  Conn.ExecuteNonQuery(
    'CREATE TABLE review_responses (' +
    '  id INTEGER PRIMARY KEY,' +
    '  review_id INTEGER UNIQUE NOT NULL REFERENCES reviews(id) ON DELETE CASCADE,' +
    '  responder TEXT NOT NULL,' +
    '  content TEXT NOT NULL,' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Review reports (for moderation)
  Conn.ExecuteNonQuery(
    'CREATE TABLE review_reports (' +
    '  id INTEGER PRIMARY KEY,' +
    '  review_id INTEGER NOT NULL REFERENCES reviews(id) ON DELETE CASCADE,' +
    '  reporter_user_id INTEGER NOT NULL REFERENCES users(id),' +
    '  reason TEXT NOT NULL,' +
    '  details TEXT,' +
    '  status TEXT DEFAULT ''pending'' CHECK(status IN (''pending'', ''reviewed'', ''action_taken'')),' +
    '  created_at TEXT DEFAULT (datetime(''now''))' +
    ')');

  // Indexes
  Conn.ExecuteNonQuery('CREATE INDEX idx_reviews_product ON reviews(product_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_reviews_user ON reviews(user_id)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_reviews_rating ON reviews(rating)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_reviews_helpful ON reviews(helpful_count DESC)');
  Conn.ExecuteNonQuery('CREATE INDEX idx_review_votes ON review_votes(review_id, user_id)');

  WriteLn('   Created tables: products, users, purchases, reviews,');
  WriteLn('                   review_votes, review_images, review_responses,');
  WriteLn('                   review_reports');
  WriteLn('');
end;

// =============================================================================
// Helper Functions
// =============================================================================
{ Recalculates and stores the rating_count, rating_sum, and rating_avg for the specified product based on its approved reviews. }
procedure UpdateProductRating(ProductId: Integer);
begin
  Conn.ExecuteNonQuery(
    'UPDATE products SET ' +
    '  rating_count = (SELECT COUNT(*) FROM reviews WHERE product_id = ? AND status = ''approved''), ' +
    '  rating_sum = (SELECT COALESCE(SUM(rating), 0) FROM reviews WHERE product_id = ? AND status = ''approved''), ' +
    '  rating_avg = (SELECT COALESCE(AVG(rating * 1.0), 0) FROM reviews WHERE product_id = ? AND status = ''approved'') ' +
    'WHERE id = ?',
    [ProductId, ProductId, ProductId, ProductId]);
end;

{ Returns a visual star rating string (e.g., "***--" for 3/5). }
function StarString(Rating: Integer): string;
begin
  Result := StringOfChar('*', Rating) + StringOfChar('-', 5 - Rating);
end;

// =============================================================================
// Review Functions
// =============================================================================
{ Adds a product review with rating, preventing duplicate reviews. }
function AddReview(ProductId, UserId, Rating: Integer; const Title, Content: string;
  const Pros: string = ''; const Cons: string = ''): Integer;
var
  IsVerified: Integer;
  V: Variant;
begin
  Result := 0;

  // Check if user already reviewed this product
  V := Conn.ExecuteScalar('SELECT id FROM reviews WHERE product_id = ? AND user_id = ?',
    [ProductId, UserId]);
  if not VarIsNull(V) then
  begin
    WriteLn('     User already reviewed this product');
    Exit;
  end;

  // Check for verified purchase
  V := Conn.ExecuteScalar('SELECT COUNT(*) FROM purchases WHERE user_id = ? AND product_id = ?',
    [UserId, ProductId]);
  if VarIsNull(V) then
    IsVerified := 0
  else if Integer(V) > 0 then
    IsVerified := 1
  else
    IsVerified := 0;

  // Add review
  Conn.ExecuteNonQuery(
    'INSERT INTO reviews (product_id, user_id, rating, title, content, pros, cons, is_verified_purchase) ' +
    'VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
    [ProductId, UserId, Rating, Title, Content, Pros, Cons, IsVerified]);

  Result := Conn.ExecuteScalar('SELECT last_insert_rowid()');

  // Update user stats
  Conn.ExecuteNonQuery('UPDATE users SET total_reviews = total_reviews + 1 WHERE id = ?', [UserId]);

  // Update product rating aggregates
  UpdateProductRating(ProductId);
end;

{ Records a helpfulness vote on a review, preventing self-votes and duplicate votes, and updates the review's helpful/not-helpful counters and the reviewer's helpful_votes_received total. }
procedure VoteReview(ReviewId, UserId: Integer; IsHelpful: Boolean);
var
  ExistingVote, ReviewUserId: Variant;
  HelpfulInt: Integer;
begin
  // Check if user already voted
  ExistingVote := Conn.ExecuteScalar('SELECT is_helpful FROM review_votes WHERE review_id = ? AND user_id = ?',
    [ReviewId, UserId]);

  // Don't allow voting on own review
  ReviewUserId := Conn.ExecuteScalar('SELECT user_id FROM reviews WHERE id = ?', [ReviewId]);
  if (not VarIsNull(ReviewUserId)) and (Integer(ReviewUserId) = UserId) then
  begin
    WriteLn('     Cannot vote on your own review');
    Exit;
  end;

  if IsHelpful then
    HelpfulInt := 1
  else
    HelpfulInt := 0;

  if VarIsNull(ExistingVote) then
  begin
    // New vote
    Conn.ExecuteNonQuery('INSERT INTO review_votes (review_id, user_id, is_helpful) VALUES (?, ?, ?)',
      [ReviewId, UserId, HelpfulInt]);

    if IsHelpful then
      Conn.ExecuteNonQuery('UPDATE reviews SET helpful_count = helpful_count + 1 WHERE id = ?', [ReviewId])
    else
      Conn.ExecuteNonQuery('UPDATE reviews SET not_helpful_count = not_helpful_count + 1 WHERE id = ?', [ReviewId]);

    // Update reviewer's helpful votes received
    if IsHelpful then
      Conn.ExecuteNonQuery(
        'UPDATE users SET helpful_votes_received = helpful_votes_received + 1 ' +
        'WHERE id = (SELECT user_id FROM reviews WHERE id = ?)', [ReviewId]);
  end;
end;

{ Inserts or replaces a seller/owner response to a specific review with the given responder name and content. }
procedure AddSellerResponse(ReviewId: Integer; const Responder, Content: string);
begin
  Conn.ExecuteNonQuery(
    'INSERT OR REPLACE INTO review_responses (review_id, responder, content) VALUES (?, ?, ?)',
    [ReviewId, Responder, Content]);
end;

// =============================================================================
// Sample Data
// =============================================================================
{ Populates the database with users, products, purchase records, reviews with ratings/pros/cons, helpfulness votes, and seller responses to negative reviews. }
procedure InsertSampleData;
var
  ReviewId: Integer;
begin
  WriteLn('2. Creating Sample Data');
  WriteLn('   ======================');

  // Create users
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''alice'', ''Alice J.'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''bob'', ''Bob S.'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''carol'', ''Carol W.'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''david'', ''David M.'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''emma'', ''Emma L.'')');
  Conn.ExecuteNonQuery('INSERT INTO users (username, display_name) VALUES (''frank'', ''Frank R.'')');

  WriteLn('   Created 6 users');

  // Create products
  Conn.ExecuteNonQuery(
    'INSERT INTO products (name, category, price, seller) ' +
    'VALUES (''Wireless Headphones Pro'', ''Electronics'', 149.99, ''TechStore'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO products (name, category, price, seller) ' +
    'VALUES (''Ergonomic Office Chair'', ''Furniture'', 299.99, ''OfficePlus'')');
  Conn.ExecuteNonQuery(
    'INSERT INTO products (name, category, price, seller) ' +
    'VALUES (''Coffee Maker Deluxe'', ''Kitchen'', 89.99, ''HomeGoods'')');

  WriteLn('   Created 3 products');

  // Create purchases (for verified purchase badges)
  Conn.ExecuteNonQuery('INSERT INTO purchases (user_id, product_id) VALUES (1, 1)'); // Alice bought headphones
  Conn.ExecuteNonQuery('INSERT INTO purchases (user_id, product_id) VALUES (2, 1)'); // Bob bought headphones
  Conn.ExecuteNonQuery('INSERT INTO purchases (user_id, product_id) VALUES (3, 2)'); // Carol bought chair
  Conn.ExecuteNonQuery('INSERT INTO purchases (user_id, product_id) VALUES (1, 3)'); // Alice bought coffee maker
  Conn.ExecuteNonQuery('INSERT INTO purchases (user_id, product_id) VALUES (5, 3)'); // Emma bought coffee maker

  WriteLn('   Created purchase records');

  // Add reviews for Wireless Headphones Pro
  WriteLn('');
  WriteLn('   Adding reviews for "Wireless Headphones Pro":');

  ReviewId := AddReview(1, 1, 5, 'Best headphones I have ever owned!',
    'The sound quality is incredible. Battery life lasts all day. Very comfortable for long sessions.',
    'Great sound, long battery, comfortable', 'A bit pricey');
  if ReviewId > 0 then WriteLn('     + Alice (5 stars, verified)');

  ReviewId := AddReview(1, 2, 4, 'Great sound, minor issues',
    'Sound quality is excellent. ANC works well. Bluetooth connection sometimes drops.',
    'Excellent audio, good ANC', 'Occasional connection issues');
  if ReviewId > 0 then WriteLn('     + Bob (4 stars, verified)');

  ReviewId := AddReview(1, 4, 3, 'Decent but overpriced',
    'They work fine but I expected more for this price. Build quality could be better.',
    'Good sound', 'Price too high, build quality');
  if ReviewId > 0 then WriteLn('     + David (3 stars)');

  ReviewId := AddReview(1, 5, 5, 'Perfect for music lovers',
    'I am an audiophile and these headphones impressed me. The detail in music is amazing.',
    'Audiophile quality, comfortable, stylish', 'None so far');
  if ReviewId > 0 then WriteLn('     + Emma (5 stars)');

  // Add reviews for Ergonomic Office Chair
  WriteLn('');
  WriteLn('   Adding reviews for "Ergonomic Office Chair":');

  ReviewId := AddReview(2, 3, 5, 'Saved my back!',
    'After years of back pain from cheap chairs, this one is a game changer. Highly adjustable.',
    'Very comfortable, great lumbar support, durable', 'Assembly takes time');
  if ReviewId > 0 then WriteLn('     + Carol (5 stars, verified)');

  ReviewId := AddReview(2, 1, 4, 'Good chair, worth the investment',
    'Quality is great. Wish the armrests were a bit more adjustable.',
    'Comfortable, good build', 'Armrests limited');
  if ReviewId > 0 then WriteLn('     + Alice (4 stars)');

  ReviewId := AddReview(2, 6, 2, 'Not as expected',
    'Chair is okay but not as comfortable as I hoped. Returning it.',
    'Looks nice', 'Not comfortable for long hours');
  if ReviewId > 0 then WriteLn('     + Frank (2 stars)');

  // Add reviews for Coffee Maker Deluxe
  WriteLn('');
  WriteLn('   Adding reviews for "Coffee Maker Deluxe":');

  ReviewId := AddReview(3, 1, 5, 'Makes amazing coffee!',
    'This coffee maker produces barista-quality espresso. Easy to use and clean.',
    'Great coffee, easy cleaning, quiet', 'Takes time to heat up');
  if ReviewId > 0 then WriteLn('     + Alice (5 stars, verified)');

  ReviewId := AddReview(3, 5, 4, 'Good value for money',
    'Makes good coffee. Not as fancy as expensive machines but does the job well.',
    'Affordable, reliable', 'Basic features only');
  if ReviewId > 0 then WriteLn('     + Emma (4 stars, verified)');

  ReviewId := AddReview(3, 3, 5, 'Love it!',
    'Best coffee maker in this price range. I use it every day.',
    'Great taste, durable, easy', 'Wish it had a timer');
  if ReviewId > 0 then WriteLn('     + Carol (5 stars)');

  // Add helpfulness votes
  WriteLn('');
  WriteLn('   Adding helpfulness votes...');
  VoteReview(1, 3, True);  // Carol found Alice's headphone review helpful
  VoteReview(1, 4, True);  // David found Alice's headphone review helpful
  VoteReview(1, 5, True);  // Emma found Alice's headphone review helpful
  VoteReview(2, 1, True);  // Alice found Bob's review helpful
  VoteReview(2, 3, True);  // Carol found Bob's review helpful
  VoteReview(3, 1, False); // Alice found David's review not helpful
  VoteReview(5, 2, True);  // Bob found Carol's chair review helpful
  VoteReview(5, 4, True);  // David found Carol's chair review helpful
  VoteReview(5, 6, True);  // Frank found Carol's chair review helpful

  // Add seller response
  WriteLn('   Adding seller responses...');
  AddSellerResponse(3, 'TechStore Support',
    'Thank you for your feedback. We stand by our quality and pricing reflects ' +
    'premium components. Feel free to contact us for any concerns.');
  AddSellerResponse(7, 'OfficePlus Team',
    'We apologize that the chair did not meet your expectations. ' +
    'Please reach out for a return or exchange. Your comfort is our priority.');

  WriteLn('');
end;

// =============================================================================
// Display Reviews
// =============================================================================
{ Prints a product summary table with average ratings and prices, then shows detailed reviews for the first product including star ratings, verified badges, pros/cons, helpfulness counts, and seller responses. }
procedure DemoProductReviews;
var
  DS: TDataSet;
begin
  WriteLn('3. Product Reviews Display');
  WriteLn('   =========================');
  WriteLn('');

  // Product summary
  DS := Conn.ExecuteQuery(
    'SELECT name, seller, rating_avg, rating_count, price FROM products ORDER BY id');
  try
    WriteLn('   Product Summary:');
    WriteLn('   Product                   | Seller      | Rating        | Reviews | Price');
    WriteLn('   --------------------------|-------------|---------------|---------|--------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-25s | %-11s | %s %.1f | %7d | $%.2f',
        [DS.FieldByName('name').AsString,
         DS.FieldByName('seller').AsString,
         StarString(Round(DS.FieldByName('rating_avg').AsFloat)),
         DS.FieldByName('rating_avg').AsFloat,
         DS.FieldByName('rating_count').AsInteger,
         DS.FieldByName('price').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Detailed reviews for first product
  WriteLn('');
  WriteLn('   Reviews for "Wireless Headphones Pro":');
  WriteLn('   ' + StringOfChar('-', 60));

  DS := Conn.ExecuteQuery(
    'SELECT r.*, u.display_name, ' +
    '  (SELECT content FROM review_responses WHERE review_id = r.id) AS response ' +
    'FROM reviews r ' +
    'JOIN users u ON r.user_id = u.id ' +
    'WHERE r.product_id = 1 AND r.status = ''approved'' ' +
    'ORDER BY r.helpful_count DESC, r.created_at DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s %s',
        [StarString(DS.FieldByName('rating').AsInteger),
         DS.FieldByName('title').AsString]));
      Write(Format('   By %s', [DS.FieldByName('display_name').AsString]));
      if DS.FieldByName('is_verified_purchase').AsInteger = 1 then
        Write(' (Verified Purchase)');
      WriteLn('');
      WriteLn(Format('   "%s"', [DS.FieldByName('content').AsString]));
      if DS.FieldByName('pros').AsString <> '' then
        WriteLn(Format('   PROS: %s', [DS.FieldByName('pros').AsString]));
      if DS.FieldByName('cons').AsString <> '' then
        WriteLn(Format('   CONS: %s', [DS.FieldByName('cons').AsString]));
      WriteLn(Format('   %d found helpful, %d not helpful',
        [DS.FieldByName('helpful_count').AsInteger,
         DS.FieldByName('not_helpful_count').AsInteger]));

      if not DS.FieldByName('response').IsNull then
      begin
        WriteLn('');
        WriteLn('   SELLER RESPONSE:');
        WriteLn(Format('   "%s"', [DS.FieldByName('response').AsString]));
      end;

      WriteLn('   ' + StringOfChar('-', 60));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Rating Distribution
// =============================================================================
{ Prints a histogram of rating distribution (5 to 1 stars) for each product, showing percentage bars and counts of approved reviews per star level. }
procedure DemoRatingDistribution;
var
  DS: TDataSet;
  Total, Count, I: Integer;
  Pct: Double;
  BarLength: Integer;
begin
  WriteLn('4. Rating Distribution');
  WriteLn('   =====================');
  WriteLn('');

  // For each product
  DS := Conn.ExecuteQuery('SELECT id, name FROM products');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('   %s:', [DS.FieldByName('name').AsString]));

      Total := Conn.ExecuteScalar(
        'SELECT COUNT(*) FROM reviews WHERE product_id = ? AND status = ''approved''',
        [DS.FieldByName('id').AsInteger]);

      if Total > 0 then
      begin
        for I := 5 downto 1 do
        begin
          Count := Conn.ExecuteScalar(
            'SELECT COUNT(*) FROM reviews WHERE product_id = ? AND rating = ? AND status = ''approved''',
            [DS.FieldByName('id').AsInteger, I]);
          Pct := (Count / Total) * 100;
          BarLength := Round(Pct / 5); // Scale to max 20 chars

          WriteLn(Format('   %d stars: %s %5.1f%% (%d)',
            [I, StringOfChar('#', BarLength) + StringOfChar(' ', 20 - BarLength), Pct, Count]));
        end;
      end
      else
        WriteLn('   No reviews yet');

      WriteLn('');
      DS.Next;
    end;
  finally
    DS.Free;
  end;
end;

// =============================================================================
// Top Reviewers
// =============================================================================
{ Displays a ranked table of reviewers showing total reviews written, helpful votes received, and average rating given, sorted by helpfulness. }
procedure DemoTopReviewers;
var
  DS: TDataSet;
begin
  WriteLn('5. Top Reviewers');
  WriteLn('   ===============');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT u.display_name, u.total_reviews, u.helpful_votes_received, ' +
    '  ROUND(AVG(r.rating), 1) AS avg_rating_given ' +
    'FROM users u ' +
    'JOIN reviews r ON u.id = r.user_id ' +
    'GROUP BY u.id ' +
    'ORDER BY u.helpful_votes_received DESC, u.total_reviews DESC');
  try
    WriteLn('   Reviewer    | Reviews | Helpful Votes | Avg Rating Given');
    WriteLn('   ------------|---------|---------------|------------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-11s | %7d | %13d | %.1f',
        [DS.FieldByName('display_name').AsString,
         DS.FieldByName('total_reviews').AsInteger,
         DS.FieldByName('helpful_votes_received').AsInteger,
         DS.FieldByName('avg_rating_given').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Verified vs Unverified
// =============================================================================
{ Compares verified vs. unverified purchase reviews showing count, average rating, and total helpful votes for each group. }
procedure DemoVerifiedAnalysis;
var
  DS: TDataSet;
begin
  WriteLn('6. Verified Purchase Analysis');
  WriteLn('   ============================');
  WriteLn('');

  DS := Conn.ExecuteQuery(
    'SELECT ' +
    '  CASE is_verified_purchase WHEN 1 THEN ''Verified'' ELSE ''Unverified'' END AS status, ' +
    '  COUNT(*) AS review_count, ' +
    '  ROUND(AVG(rating), 2) AS avg_rating, ' +
    '  SUM(helpful_count) AS total_helpful ' +
    'FROM reviews ' +
    'WHERE status = ''approved'' ' +
    'GROUP BY is_verified_purchase');
  try
    WriteLn('   Status     | Reviews | Avg Rating | Helpful Votes');
    WriteLn('   -----------|---------|------------|---------------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-10s | %7d | %10.2f | %d',
        [DS.FieldByName('status').AsString,
         DS.FieldByName('review_count').AsInteger,
         DS.FieldByName('avg_rating').AsFloat,
         DS.FieldByName('total_helpful').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
  WriteLn('   Insight: Verified purchasers tend to rate higher and receive more helpful votes.');
  WriteLn('');
end;

// =============================================================================
// Review Sorting Options
// =============================================================================
{ Displays reviews sorted by most helpful votes across all products, then lists critical reviews (1-2 stars) in chronological order with content excerpts. }
procedure DemoReviewSorting;
var
  DS: TDataSet;
begin
  WriteLn('7. Review Sorting Options');
  WriteLn('   ========================');
  WriteLn('');

  // Most helpful reviews
  WriteLn('   Most Helpful Reviews (all products):');
  DS := Conn.ExecuteQuery(
    'SELECT r.title, r.rating, u.display_name, r.helpful_count, p.name AS product ' +
    'FROM reviews r ' +
    'JOIN users u ON r.user_id = u.id ' +
    'JOIN products p ON r.product_id = p.id ' +
    'WHERE r.status = ''approved'' ' +
    'ORDER BY r.helpful_count DESC ' +
    'LIMIT 5');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s "%s" by %s - %d helpful',
        [StarString(DS.FieldByName('rating').AsInteger),
         DS.FieldByName('title').AsString,
         DS.FieldByName('display_name').AsString,
         DS.FieldByName('helpful_count').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Critical reviews (low ratings)
  WriteLn('');
  WriteLn('   Critical Reviews (1-2 stars):');
  DS := Conn.ExecuteQuery(
    'SELECT r.title, r.rating, u.display_name, p.name AS product, r.content ' +
    'FROM reviews r ' +
    'JOIN users u ON r.user_id = u.id ' +
    'JOIN products p ON r.product_id = p.id ' +
    'WHERE r.status = ''approved'' AND r.rating <= 2 ' +
    'ORDER BY r.created_at DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %s [%s] "%s"',
        [StarString(DS.FieldByName('rating').AsInteger),
         DS.FieldByName('product').AsString,
         DS.FieldByName('title').AsString]));
      WriteLn(Format('       "%s"', [Copy(DS.FieldByName('content').AsString, 1, 60)]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Review Statistics
// =============================================================================
{ Demonstrates review statistics including averages and distributions. }
procedure DemoStatistics;
var
  TotalReviews, TotalVotes, VerifiedCount: Integer;
  AvgRating: Double;
  DS: TDataSet;
begin
  WriteLn('8. Review Statistics');
  WriteLn('   ===================');
  WriteLn('');

  TotalReviews := Conn.ExecuteScalar('SELECT COUNT(*) FROM reviews WHERE status = ''approved''');
  TotalVotes := Conn.ExecuteScalar('SELECT COUNT(*) FROM review_votes');
  VerifiedCount := Conn.ExecuteScalar('SELECT COUNT(*) FROM reviews WHERE is_verified_purchase = 1');
  AvgRating := Conn.ExecuteScalar('SELECT AVG(rating) FROM reviews WHERE status = ''approved''');

  WriteLn(Format('   Total approved reviews: %d', [TotalReviews]));
  WriteLn(Format('   Total helpfulness votes: %d', [TotalVotes]));
  WriteLn(Format('   Verified purchase reviews: %d (%.1f%%)',
    [VerifiedCount, (VerifiedCount / TotalReviews) * 100]));
  WriteLn(Format('   Overall average rating: %.2f', [AvgRating]));

  // Rating breakdown
  WriteLn('');
  WriteLn('   Overall rating breakdown:');
  DS := Conn.ExecuteQuery(
    'SELECT rating, COUNT(*) AS cnt, ' +
    '  ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM reviews WHERE status = ''approved''), 1) AS pct ' +
    'FROM reviews WHERE status = ''approved'' ' +
    'GROUP BY rating ORDER BY rating DESC');
  try
    while not DS.EOF do
    begin
      WriteLn(Format('     %d stars: %d reviews (%.1f%%)',
        [DS.FieldByName('rating').AsInteger,
         DS.FieldByName('cnt').AsInteger,
         DS.FieldByName('pct').AsFloat]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Seller Dashboard
// =============================================================================
{ Shows per-seller performance metrics (product count, reviews, average rating, positive/negative breakdown) and identifies negative reviews still awaiting a seller response. }
procedure DemoSellerDashboard;
var
  DS: TDataSet;
begin
  WriteLn('9. Seller Dashboard');
  WriteLn('   ==================');
  WriteLn('');

  WriteLn('   Performance by Seller:');
  DS := Conn.ExecuteQuery(
    'SELECT p.seller, ' +
    '  COUNT(DISTINCT p.id) AS products, ' +
    '  COUNT(r.id) AS total_reviews, ' +
    '  ROUND(AVG(r.rating), 2) AS avg_rating, ' +
    '  SUM(CASE WHEN r.rating >= 4 THEN 1 ELSE 0 END) AS positive, ' +
    '  SUM(CASE WHEN r.rating <= 2 THEN 1 ELSE 0 END) AS negative ' +
    'FROM products p ' +
    'LEFT JOIN reviews r ON p.id = r.product_id AND r.status = ''approved'' ' +
    'GROUP BY p.seller');
  try
    WriteLn('   Seller      | Products | Reviews | Avg Rating | Positive | Negative');
    WriteLn('   ------------|----------|---------|------------|----------|----------');
    while not DS.EOF do
    begin
      WriteLn(Format('   %-11s | %8d | %7d | %10.2f | %8d | %d',
        [DS.FieldByName('seller').AsString,
         DS.FieldByName('products').AsInteger,
         DS.FieldByName('total_reviews').AsInteger,
         DS.FieldByName('avg_rating').AsFloat,
         DS.FieldByName('positive').AsInteger,
         DS.FieldByName('negative').AsInteger]));
      DS.Next;
    end;
  finally
    DS.Free;
  end;

  // Reviews needing attention (negative, no response)
  WriteLn('');
  WriteLn('   Reviews needing attention (negative, no seller response):');
  DS := Conn.ExecuteQuery(
    'SELECT p.name, r.rating, r.title, u.display_name ' +
    'FROM reviews r ' +
    'JOIN products p ON r.product_id = p.id ' +
    'JOIN users u ON r.user_id = u.id ' +
    'WHERE r.rating <= 2 AND r.status = ''approved'' ' +
    '  AND r.id NOT IN (SELECT review_id FROM review_responses) ' +
    'ORDER BY r.created_at DESC');
  try
    if DS.EOF then
      WriteLn('     All negative reviews have been responded to!')
    else
    begin
      while not DS.EOF do
      begin
        WriteLn(Format('     [%s] %s - "%s" by %s',
          [DS.FieldByName('name').AsString,
           StarString(DS.FieldByName('rating').AsInteger),
           DS.FieldByName('title').AsString,
           DS.FieldByName('display_name').AsString]));
        DS.Next;
      end;
    end;
  finally
    DS.Free;
  end;

  WriteLn('');
end;

// =============================================================================
// Main
// =============================================================================
begin
  WriteLn('');
  WriteLn('=== NDXSQLite Example 77: Ratings and Reviews System ===');
  WriteLn('');

  Conn := TNDXSQLiteConnection.Create(':memory:');
  try
    Conn.Open;

    CreateSchema;
    InsertSampleData;
    DemoProductReviews;
    DemoRatingDistribution;
    DemoTopReviewers;
    DemoVerifiedAnalysis;
    DemoReviewSorting;
    DemoStatistics;
    DemoSellerDashboard;

    Conn.Close;
  finally
    Conn.Free;
  end;

  WriteLn('=== Example completed successfully! ===');
  WriteLn('');
end.
